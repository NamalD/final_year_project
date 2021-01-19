package execute;

import model.GameModel;
import view.ManagerView;

public class RunManager {

  /**
   * Run program with GUI.
   * 
   * @param args
   *          Does nothing in current state.
   */
  public static void main(String[] args) {
    GameModel model = new GameModel();
    @SuppressWarnings("unused")
    ManagerView view = new ManagerView(model);
  }

}
