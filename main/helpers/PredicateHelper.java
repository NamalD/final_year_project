package helpers;

import java.util.List;

public class PredicateHelper {

  /**
   * Get predicate relating to who has control. 2 player games are implied,
   * which is why only first found predicate is returned.
   * 
   * @param statePredicates List of predicates that currently hold.
   * @return    Control predicate in the form: 'control(roleID)', or null if no-one is in control.
   */
  public static String getControl(List<String> statePredicates) {
    // TODO Refactor so all held control predicates returned.
    for (String predicate : statePredicates) {
      if (predicate.startsWith("control")) {
        return predicate;
      }
    }
    return null;
  }

  /**
   * Turn a set of predicates into contents of 2d grid.
   * 
   * @param statePredicates
   *          Set of predicates to parse
   * @return 2D String array of contents
   */
  // ONLY WORKS FOR PREDICATES IN FORM: cell(X, Y, M)
  public static String[][] predicatesToCells(List<String> statePredicates) {
    String[][] contents = new String[3][3];
    for (String predicate : statePredicates) {
      // Wanted predicates: cell(X, Y, M)
      if (predicate.startsWith("cell")) {
        String[] formatted = 
            predicate.substring(5, predicate.length() - 1).replace(" ", "").split(",");
        Integer indX = Integer.parseInt(formatted[0]) - 1;
        Integer indY = Integer.parseInt(formatted[1]) - 1;
        contents[indX][indY] = formatted[2];
      }
    }
    return contents;
  }

}
