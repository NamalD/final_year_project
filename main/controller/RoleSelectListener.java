package controller;

import model.GameModel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class RoleSelectListener extends BaseGameListener implements ActionListener {

  public RoleSelectListener(GameModel model) {
    super(model);
  }

  @Override
  public void actionPerformed(ActionEvent ev) {
    String[] formatted = ev.getActionCommand().split(":");
    this.model.assignPlayer(formatted[0], formatted[1]);
  }

}
