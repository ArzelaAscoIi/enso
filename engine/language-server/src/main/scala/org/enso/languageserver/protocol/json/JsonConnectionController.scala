package org.enso.languageserver.protocol.json

import akka.actor.{Actor, ActorRef, Props, Stash, Status}
import akka.pattern.pipe
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.boot.resource.InitializationComponent
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.data.Config
import org.enso.languageserver.event.{
  JsonSessionInitialized,
  JsonSessionTerminated
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager.PathWatcherProtocol
import org.enso.languageserver.io.InputOutputApi._
import org.enso.languageserver.io.OutputKind.{StandardError, StandardOutput}
import org.enso.languageserver.io.{InputOutputApi, InputOutputProtocol}
import org.enso.languageserver.monitoring.MonitoringApi.{InitialPing, Ping}
import org.enso.languageserver.refactoring.RefactoringApi.RenameProject
import org.enso.languageserver.requesthandler._
import org.enso.languageserver.requesthandler.capability._
import org.enso.languageserver.requesthandler.io._
import org.enso.languageserver.requesthandler.monitoring.{
  InitialPingHandler,
  PingHandler
}
import org.enso.languageserver.requesthandler.refactoring.RenameProjectHandler
import org.enso.languageserver.requesthandler.session.InitProtocolConnectionHandler
import org.enso.languageserver.requesthandler.text._
import org.enso.languageserver.requesthandler.visualisation.{
  AttachVisualisationHandler,
  DetachVisualisationHandler,
  ExecuteExpressionHandler,
  ModifyVisualisationHandler
}
import org.enso.languageserver.requesthandler.workspace.ProjectInfoHandler
import org.enso.languageserver.runtime.ContextRegistryProtocol
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.VisualisationApi.{
  AttachVisualisation,
  DetachVisualisation,
  ExecuteExpression,
  ModifyVisualisation
}
import org.enso.languageserver.search.SearchApi._
import org.enso.languageserver.search.{SearchApi, SearchProtocol}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionApi.{
  InitProtocolConnection,
  ResourcesInitializationError,
  SessionAlreadyInitialisedError,
  SessionNotInitialisedError
}
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.workspace.WorkspaceApi.ProjectInfo

import java.util.UUID
import scala.concurrent.duration._

/** An actor handling communications between a single client and the language
  * server.
  *
  * @param connectionId the internal connection id
  * @param mainComponent the main initialization logic
  * @param bufferRegistry a router that dispatches text editing requests
  * @param capabilityRouter a router that dispatches capability requests
  * @param fileManager performs operations with file system
  * @param contextRegistry a router that dispatches execution context requests
  * @param suggestionsHandler a reference to the suggestions requests handler
  * @param requestTimeout a request timeout
  */
class JsonConnectionController(
  val connectionId: UUID,
  val mainComponent: InitializationComponent,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  val fileManager: ActorRef,
  val contextRegistry: ActorRef,
  val suggestionsHandler: ActorRef,
  val stdOutController: ActorRef,
  val stdErrController: ActorRef,
  val stdInController: ActorRef,
  val runtimeConnector: ActorRef,
  val languageServerConfig: Config,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(requestTimeout)

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  private def connected(webActor: ActorRef): Receive = {
    case req @ Request(Ping, _, Unused) =>
      val handler = context.actorOf(
        PingHandler.props(
          List(
            bufferRegistry,
            capabilityRouter,
            fileManager,
            contextRegistry
          ),
          requestTimeout
        )
      )
      handler.forward(req)

    case req @ Request(RenameProject, _, _) =>
      val handler = context.actorOf(
        RenameProjectHandler.props(requestTimeout, runtimeConnector)
      )
      handler.forward(req)

    case req @ Request(
          InitProtocolConnection,
          _,
          InitProtocolConnection.Params(clientId)
        ) =>
      logger.info("Initializing resources.")
      mainComponent.init().pipeTo(self)
      context.become(initializing(webActor, clientId, req, sender()))

    case Request(_, id, _) =>
      sender() ! ResponseError(Some(id), SessionNotInitialisedError)

    case MessageHandler.Disconnected =>
      context.stop(self)
  }

  private def initializing(
    webActor: ActorRef,
    clientId: UUID,
    request: Request[_, _],
    receiver: ActorRef
  ): Receive = {
    case InitializationComponent.Initialized =>
      logger.info("RPC session initialized for client [{}].", clientId)
      val session = JsonSession(clientId, self)
      context.system.eventStream.publish(JsonSessionInitialized(session))
      val requestHandlers = createRequestHandlers(session)
      val handler = context.actorOf(
        InitProtocolConnectionHandler.props(fileManager, requestTimeout)
      )
      handler.tell(request, receiver)
      unstashAll()
      context.become(initialised(webActor, session, requestHandlers))

    case Status.Failure(ex) =>
      logger.error("Failed to initialize the resources. {}", ex.getMessage)
      receiver ! ResponseError(Some(request.id), ResourcesInitializationError)
      context.become(connected(webActor))

    case _ => stash()
  }

  private def initialised(
    webActor: ActorRef,
    rpcSession: JsonSession,
    requestHandlers: Map[Method, Props]
  ): Receive = {
    case Request(InitProtocolConnection, id, _) =>
      sender() ! ResponseError(Some(id), SessionAlreadyInitialisedError)

    case MessageHandler.Disconnected =>
      context.system.eventStream.publish(JsonSessionTerminated(rpcSession))
      context.stop(self)

    case CapabilityProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case CapabilityProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case TextProtocol.TextDidChange(changes) =>
      webActor ! Notification(TextDidChange, TextDidChange.Params(changes))

    case PathWatcherProtocol.FileEventResult(event) =>
      webActor ! Notification(
        EventFile,
        EventFile.Params(event.path, event.kind)
      )

    case ContextRegistryProtocol
          .ExpressionUpdatesNotification(contextId, updates) =>
      webActor ! Notification(
        ExecutionContextExpressionUpdates,
        ExecutionContextExpressionUpdates.Params(contextId, updates)
      )

    case ContextRegistryProtocol.ExecutionFailedNotification(
          contextId,
          failure
        ) =>
      webActor ! Notification(
        ExecutionContextExecutionFailed,
        ExecutionContextExecutionFailed.Params(
          contextId,
          failure.message,
          failure.path
        )
      )

    case ContextRegistryProtocol.ExecutionDiagnosticNotification(
          contextId,
          diagnostics
        ) =>
      webActor ! Notification(
        ExecutionContextExecutionStatus,
        ExecutionContextExecutionStatus.Params(contextId, diagnostics)
      )

    case ContextRegistryProtocol.VisualisationEvaluationFailed(
          contextId,
          visualisationId,
          expressionId,
          message,
          diagnostic
        ) =>
      webActor ! Notification(
        VisualisationEvaluationFailed,
        VisualisationEvaluationFailed.Params(
          contextId,
          visualisationId,
          expressionId,
          message,
          diagnostic
        )
      )

    case SearchProtocol.SuggestionsDatabaseUpdateNotification(
          version,
          updates
        ) =>
      webActor ! Notification(
        SearchApi.SuggestionsDatabaseUpdates,
        SearchApi.SuggestionsDatabaseUpdates.Params(updates, version)
      )

    case InputOutputProtocol.OutputAppended(output, outputKind) =>
      outputKind match {
        case StandardOutput =>
          webActor ! Notification(
            StandardOutputAppended,
            StandardOutputAppended.Params(output)
          )

        case StandardError =>
          webActor ! Notification(
            StandardErrorAppended,
            StandardErrorAppended.Params(output)
          )

      }

    case InputOutputProtocol.WaitingForStandardInput =>
      webActor ! Notification(InputOutputApi.WaitingForStandardInput, Unused)

    case req @ Request(method, _, _) if requestHandlers.contains(method) =>
      val handler = context.actorOf(
        requestHandlers(method),
        s"request-handler-$method-${UUID.randomUUID()}"
      )
      handler.forward(req)
  }

  private def createRequestHandlers(
    rpcSession: JsonSession
  ): Map[Method, Props] = {
    Map(
      Ping -> PingHandler.props(
        List(
          bufferRegistry,
          capabilityRouter,
          fileManager,
          contextRegistry
        ),
        requestTimeout
      ),
      InitialPing -> InitialPingHandler.props,
      AcquireCapability -> AcquireCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      ReleaseCapability -> ReleaseCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      OpenFile -> OpenFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      CloseFile -> CloseFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      ApplyEdit -> ApplyEditHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      SaveFile -> SaveFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      WriteFile -> file.WriteTextualFileHandler
        .props(requestTimeout, fileManager),
      ReadFile -> file.ReadTextualFileHandler
        .props(requestTimeout, fileManager),
      CreateFile -> file.CreateFileHandler.props(requestTimeout, fileManager),
      DeleteFile -> file.DeleteFileHandler.props(requestTimeout, fileManager),
      CopyFile   -> file.CopyFileHandler.props(requestTimeout, fileManager),
      MoveFile   -> file.MoveFileHandler.props(requestTimeout, fileManager),
      ExistsFile -> file.ExistsFileHandler.props(requestTimeout, fileManager),
      ListFile   -> file.ListFileHandler.props(requestTimeout, fileManager),
      TreeFile   -> file.TreeFileHandler.props(requestTimeout, fileManager),
      InfoFile   -> file.InfoFileHandler.props(requestTimeout, fileManager),
      ChecksumFile -> file.ChecksumFileHandler
        .props(requestTimeout, fileManager),
      ExecutionContextCreate -> executioncontext.CreateHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextDestroy -> executioncontext.DestroyHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextPush -> executioncontext.PushHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextPop -> executioncontext.PopHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextRecompute -> executioncontext.RecomputeHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      GetSuggestionsDatabaseVersion -> search.GetSuggestionsDatabaseVersionHandler
        .props(requestTimeout, suggestionsHandler),
      GetSuggestionsDatabase -> search.GetSuggestionsDatabaseHandler
        .props(requestTimeout, suggestionsHandler),
      InvalidateSuggestionsDatabase -> search.InvalidateSuggestionsDatabaseHandler
        .props(requestTimeout, suggestionsHandler),
      Completion -> search.CompletionHandler
        .props(requestTimeout, suggestionsHandler),
      Import -> search.ImportHandler.props(requestTimeout, suggestionsHandler),
      ExecuteExpression -> ExecuteExpressionHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      AttachVisualisation -> AttachVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      DetachVisualisation -> DetachVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      ModifyVisualisation -> ModifyVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      RedirectStandardOutput -> RedirectStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardOutput -> SuppressStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardError -> SuppressStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      RedirectStandardError -> RedirectStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      FeedStandardInput -> FeedStandardInputHandler.props(stdInController),
      ProjectInfo       -> ProjectInfoHandler.props(languageServerConfig)
    )
  }

}

object JsonConnectionController {

  /** Creates a configuration object used to create a [[JsonConnectionController]].
    *
    * @param connectionId the internal connection id
    * @param mainComponent the main initialization logic
    * @param bufferRegistry a router that dispatches text editing requests
    * @param capabilityRouter a router that dispatches capability requests
    * @param fileManager performs operations with file system
    * @param contextRegistry a router that dispatches execution context requests
    * @param suggestionsHandler a reference to the suggestions requests handler
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props(
    connectionId: UUID,
    mainComponent: InitializationComponent,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileManager: ActorRef,
    contextRegistry: ActorRef,
    suggestionsHandler: ActorRef,
    stdOutController: ActorRef,
    stdErrController: ActorRef,
    stdInController: ActorRef,
    runtimeConnector: ActorRef,
    languageServerConfig: Config,
    requestTimeout: FiniteDuration = 10.seconds
  ): Props =
    Props(
      new JsonConnectionController(
        connectionId,
        mainComponent,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry,
        suggestionsHandler,
        stdOutController,
        stdErrController,
        stdInController,
        runtimeConnector,
        languageServerConfig,
        requestTimeout
      )
    )

}
