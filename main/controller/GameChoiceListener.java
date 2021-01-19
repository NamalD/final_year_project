package controller;

import enums.Game;
import model.GameModel;

import java.awt.event.ActionEvent;


public class GameChoiceListener extends BaseGameListener {

  public GameChoiceListener(GameModel model) {
    super(model);
  }

  // Change model's selected game
  @Override
  public void actionPerformed(ActionEvent ev) {
    model.setSelectedGame(Game.fromName(ev.getActionCommand()));
  }

}
