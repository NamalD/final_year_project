package controller;

import model.GameModel;

import java.awt.event.ActionEvent;


/**
 * Listener to deal with start button presses.
 */
public class StartListener extends BaseGameListener {

  public StartListener(GameModel model) {
    super(model);
  }

  @Override
  public void actionPerformed(ActionEvent ev) {
    this.model.playGame();
  }

}
