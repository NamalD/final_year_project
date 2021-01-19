package model;

import enums.Game;
import enums.Player;
import helpers.LogicConnector;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Observable;
import java.util.concurrent.SynchronousQueue;

import javax.swing.JOptionPane;

public class GameModel extends Observable {
  private LogicConnector connectorHelper;
  private List<String> currentStatePreds;
  private int currentTime;
  private Thread gameThread;
  private HashMap<String, Player> rolePlayer;
  private List<String> roles;
  private Game selectedGame;
  private SynchronousQueue<String> syncQueue;
  private volatile String userMove = null;

  private boolean waitingForUser;

  /**
   * Constructor loads all required files.
   */
  public GameModel() {
    // Load files
    ArrayList<File> loadFiles = new ArrayList<File>();
    // Manager and temporal predicates
    loadFiles.add(new File("logic/manager.pl"));
    loadFiles.add(new File("logic/temporalPredicates.pl"));
    // Decision making players
    for (Player d : Player.values()) {
      if (d.getFileLocation() != null) {
        loadFiles.add(new File(d.getFileLocation()));
      }
    }

    // Load all files into prolog process
    connectorHelper = new LogicConnector(loadFiles.toArray(new File[loadFiles.size()]));

    // Initialisations
    currentTime = 0;
    rolePlayer = new HashMap<String, Player>();
    waitingForUser = false;
  }

  /**
   * Send a notification, asking user to make a move.
   */
  public void askUserMove() {
    setChanged();
    notifyObservers("make_move");
  }

  /**
   * Match role to player.
   * 
   * @param role
   *          Game role
   * @param player
   *          Decision making player.
   */
  public void assignPlayer(String role, String player) {
    // Keep track within the model of who plays which roles
    rolePlayer.put(role, Player.fromString(player));
  }

  /**
   * Fetch game state and store it. Sends update to observers.
   */
  public void fetchGameState() {
    currentStatePreds = connectorHelper.getStateAt(currentTime);

    setChanged();
    notifyObservers("state");
  }

  public void fetchRoles() {
    setRoles(connectorHelper.fetchRoles());
  }

  public Thread getGameThread() {
    return gameThread;
  }

  public HashMap<String, Player> getRolePlayer() {
    return rolePlayer;
  }

  public List<String> getRoles() {
    return roles;
  }

  public Game getSelectedGame() {
    return selectedGame;
  }

  public List<String> getStatePredicates() {
    return currentStatePreds;
  }

  public SynchronousQueue<String> getSyncQueue() {
    return this.syncQueue;
  }

  public String getUserMove() {
    return userMove;
  }

  /**
   * Check if game is in terminal state. Send notification if so.
   * 
   * @return True if in terminal state, false otherwise.
   */
  public boolean isTerminal() {
    if (connectorHelper.gameIsTerminal(currentTime)) {
      setChanged();
      notifyObservers("terminal");
      return true;
    } else {
      return false;
    }
  }

  public boolean isWaitingForUser() {
    return waitingForUser;
  }

  /**
   * Consult game file.
   * 
   * @param game
   *          Game to load
   */
  public void loadGame(Game game) {
    connectorHelper.consultFile(new File(game.getFileLocation()));
  }

  /**
   * Check game can run, then do so.
   */
  public void playGame() {
    // Send warning if a game has not been selected
    if (selectedGame == null) {
      String message = "You must select a game";
      JOptionPane.showMessageDialog(null, message, "Missing Game", JOptionPane.ERROR_MESSAGE);
    } else if (getRolePlayer().keySet().size() != roles.size()) {
      // Send warning if there are not enough players assigned to a role
      String message = "Not all roles have been assigned a player";
      JOptionPane.showMessageDialog(null, message, "Missing players", JOptionPane.ERROR_MESSAGE);
    } else {
      // Finalise role/player relation
      connectorHelper.applyUserRole(getRolePlayer());
      // Game is run as a thread
      gameThread = new Thread(new GameThread(this, connectorHelper));
      gameThread.start();
    }
  }

  /**
   * Setter for player roles. Notifies and updates.
   * 
   * @param roles
   *          New roles.
   */
  public void setRoles(List<String> roles) {
    this.roles = roles;
    setChanged();
    notifyObservers("roles");
  }

  /**
   * Setter for selected game.
   * 
   * @param selectedGame
   *          Game to be selected.
   */
  public void setSelectedGame(Game selectedGame) {
    this.selectedGame = selectedGame;
    // Auto load game
    loadGame(selectedGame);
    // Also fetch roles for selected game
    fetchRoles();
  }

  public void setSyncQueue(SynchronousQueue<String> queue) {
    this.syncQueue = queue;
  }

  public void setTime(int time) {
    this.currentTime = time;
  }

  public void setUserMove(String move) {
    this.userMove = move;
  }
}
