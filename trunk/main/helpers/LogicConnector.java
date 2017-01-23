
package helpers;

import enums.Player;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class LogicConnector {
  private static PrologProcess process;

  /**
   * Basic constructor initialises connection to process.
   */
  public LogicConnector() {
    try {
      process = Connector.newPrologProcess();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Overloaded constructor initialises connection and consults given file.
   * 
   * @param file
   *          File to immediately consult.
   */
  public LogicConnector(final File file) {
    try {
      process = Connector.newPrologProcess();
      process.consult(file);
    } catch (final Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Load multiple files within constructor.
   * 
   * @param files
   *          Array of Files to load.
   */
  public LogicConnector(File[] files) {
    this();
    for (File file : files) {
      consultFile(file);
    }
  }

  /**
   * Call pre-built manager predicate: 'ask_player(Player, Role, CurrentTime)'
   * which takes care of everything.
   * 
   * @param currentTime
   *          Time at which moves apply.
   * @param nonHumanRoles
   *          HashMap of non human roles.
   */
  public void applyNonHumanMoves(int currentTime, HashMap<String, Player> nonHumanRoles) {
    for (Entry<String, Player> el : nonHumanRoles.entrySet()) {
      String handleQ = QueryUtils.bT("ask_player", el.getValue().name().toLowerCase(),
          el.getKey(), currentTime);

      try {
        process.queryOnce(handleQ);
      } catch (PrologProcessException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * Handle user action by calling pre-built predicate :
   * 'handle_transforms(Role, Action, Time)'.
   * 
   * @param role
   *          Role performing action.
   * @param userMove
   *          Action being performed.
   * @param time
   *          Time when action called.
   */
  public void applyUserMove(String role, String userMove, int time) {
    String transformQ = QueryUtils.bT("handle_transforms", role, userMove, time);
    try {
      process.queryOnce(transformQ);
    } catch (PrologProcessException e) {
      e.printStackTrace();
    }
  }

  /**
   * Assert relation between a role and a player type. Form example:
   * role_player(xplayer, random).
   * 
   * @param hashMap
   *          Map of roles to players.
   */
  public void applyUserRole(HashMap<String, Player> hashMap) {
    for (Entry<String, Player> e : hashMap.entrySet()) {
      try {
        String relTerm = e.getKey() + "," + e.getValue().name().toLowerCase();
        process.queryOnce("assertz(role_player(" + relTerm + "))");
      } catch (PrologProcessException e1) {
        e1.printStackTrace();
      }
    }
  }

  /**
   * Consult a single file.
   * 
   * @param file
   *          File to consult.
   */
  public void consultFile(File file) {
    try {
      process.consult(file);
    } catch (PrologProcessException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get list of roles for loaded game.
   * 
   * @return List of roles
   */
  public List<String> fetchRoles() {
    String rolesQuery = QueryUtils.bT("roles", "R");
    List<Map<String, Object>> results = performQuery(rolesQuery);

    String result = results.get(0).get("R").toString();

    // Result in the form: String :: "[roleOne, roleTwo]"
    // Wanted: List<String> :: {"roleOne", "roleTwo"}
    String[] formatted = result.substring(1, result.length() - 1).replace(" ", "").split(",");

    return Arrays.asList(formatted);
  }

  /**
   * Check if the game is terminal at a given time.
   * 
   * @param time
   *          Game time to be checked, usually the current game time.
   * @return True if the game is terminal, false otherwise
   */
  public boolean gameIsTerminal(int time) {
    String terminalQ = QueryUtils.bT("terminal", time);

    try {
      if (process.queryOnce(terminalQ) == null) {
        return false;
      }
    } catch (PrologProcessException e) {
      e.printStackTrace();
    }

    return true;
  }

  /**
   * Get list of legal moves.
   * 
   * @param role
   *          Applicable role
   * @param time
   *          Current time
   * @return List of legal moves.
   */
  @SuppressWarnings("unchecked")
  public ArrayList<String> getLegalMoves(String role, int time) {
    // Query returns list objects
    String legalQ = QueryUtils.bT("get_legal_moves", role, time, "L");
    List<Map<String, Object>> results = null;

    try {
      results = process.queryAll(legalQ);
    } catch (PrologProcessException e) {
      e.printStackTrace();
    }

    ArrayList<String> out = new ArrayList<String>();
    for (Map<String, Object> result : results) {
      out.addAll((Collection<? extends String>) result.get("L"));
    }

    return out;
  }

  /**
   * Get the state predicates which hold at given time.
   * 
   * @param time
   *          Time of state
   * @return List of predicates which hold.
   */
  public List<String> getStateAt(int time) {
    String stateQuery = QueryUtils.bT("get_state", "Pred", time);

    List<Map<String, Object>> results = performQuery(stateQuery);

    List<String> statePreds = new ArrayList<String>();

    for (Map<String, Object> result : results) {
      statePreds.add(result.get("Pred").toString());
    }

    return statePreds;
  }

  /**
   * Perform query that returns multiple answers.
   * 
   * @param query
   *          Query that returns multiple answers
   * @return Results of query
   */
  private List<Map<String, Object>> performQuery(String query) {
    List<Map<String, Object>> results = null;

    try {
      results = process.queryAll(query);
    } catch (PrologProcessException e) {
      e.printStackTrace();
    }
    return results;
  }

  /**
   * Check if predicate exists in loaded set.
   * 
   * @param predicate
   *          Predicate to check.
   * @return True if predicate exists, false otherwise
   */
  public boolean predicateExists(final String predicate) {
    // Check predicate exists
    String existPQuery = QueryUtils.bT("current_predicate", predicate);
    try {
      if (process.queryOnce(existPQuery) != null) {
        return true;
      }
      ;
    } catch (Exception e) {
      e.printStackTrace();
    }
    return false;
  }

}
