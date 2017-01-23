package model;

import enums.Player;
import helpers.LogicConnector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.SynchronousQueue;

public class GameThread implements Runnable {
  private LogicConnector connectorHelper;
  private int currentTime;
  private GameModel model;
  private SynchronousQueue<String> syncQueue;

  private String userMove;

  /**
   * Constructor for passing references from model.
   * 
   * @param model
   *          Game Model
   * @param conHelper
   *          Pre-loaded Logic Connector
   */
  public GameThread(GameModel model, LogicConnector conHelper) {
    this.model = model;
    this.connectorHelper = conHelper;
    currentTime = 0;
    syncQueue = new SynchronousQueue<String>();
    model.setSyncQueue(syncQueue);
  }

  public SynchronousQueue<String> getSyncQueue() {
    return syncQueue;
  }

  @Override
  public void run() {
    // Setup map of roles
    HashMap<String, Player> allRoles = model.getRolePlayer();
    HashMap<String, Player> humanRoles = new HashMap<String, Player>();

    for (Entry<String, Player> e : model.getRolePlayer().entrySet()) {
      if (e.getValue().equals(Player.Human)) {
        humanRoles.put(e.getKey(), e.getValue());
      }
    }

    // Non-human roles
    // Create copy of total roles then subtract human roles
    HashMap<String, Player> nonHumanRoles = new HashMap<String, Player>(allRoles);
    for (String humanKey : humanRoles.keySet()) {
      nonHumanRoles.remove(humanKey);
    }

    // Constant loop until game is in terminal state
    while (!model.isTerminal()) {

      // Deal with human player(s):
      for (String role : humanRoles.keySet()) {
        
        ArrayList<String> legalMoves = connectorHelper.getLegalMoves(role, currentTime);

        System.out.println(currentTime + ": " + legalMoves);

        // Perform move for user if there is only one option
        if (legalMoves.size() == 1) {
          connectorHelper.applyUserMove(role, legalMoves.get(0), currentTime);
        } else {
          // Retrieve move from user

          if (userMove == null) {
            // Tell model to ask for move through notification
            model.askUserMove();

            // Take element from queue until !null element gotten
            try {
              userMove = syncQueue.take();
            } catch (InterruptedException e1) {
              e1.printStackTrace();
            }
          }

          // Apply user move to prolog connection
          connectorHelper.applyUserMove(role, userMove, currentTime);
        }
      }
      // Non-Human player(s):
      // Get non-human moves using prolog queries and apply
      connectorHelper.applyNonHumanMoves(currentTime, nonHumanRoles);

      // Progress to next game step
      // Increment time
      currentTime++;

      // Update model information
      model.setTime(currentTime);
      model.fetchGameState();
    }

    // TODO Deal with post-terminal effects.
    // END:
    // Send message
    // Tell reward values
  }

}
