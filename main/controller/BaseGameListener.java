package controller;

import model.GameModel;

import java.awt.event.ActionListener;


/**
 * Abstract class for making specialised game listeners.
 */
public abstract class BaseGameListener implements ActionListener {
  GameModel model;

  public BaseGameListener(GameModel model) {
    this.model = model;
  }

}
