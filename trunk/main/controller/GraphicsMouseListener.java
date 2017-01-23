package controller;

import model.GameModel;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;


public class GraphicsMouseListener implements MouseListener {

  private GameModel model;
  private boolean moveAsked;

  public GraphicsMouseListener(GameModel model) {
    this.model = model;
  }

  @Override
  public void mouseClicked(MouseEvent me) {
    if (moveAsked) {
      int indX = (int) (3.0 * me.getX() / 500) + 1;
      int indY = (int) (3.0 * me.getY() / 500) + 1;

      // TIC-TAC-TOE SPECIFIC ACTION
      String action = "mark(" + indX + "," + indY + ")";

      // Add to blocked queue (dealt with by thread in model)
      model.getSyncQueue().offer(action);

      // Reset move asked
      setMoveAsked(false);
    }
  }

  @Override
  public void mouseEntered(MouseEvent ev) {
    // TODO Auto-generated method stub

  }

  @Override
  public void mouseExited(MouseEvent ev) {
    // TODO Auto-generated method stub

  }

  @Override
  public void mousePressed(MouseEvent ev) {
    // TODO Auto-generated method stub

  }

  @Override
  public void mouseReleased(MouseEvent ev) {
    // TODO Auto-generated method stub

  }

  public void setMoveAsked(boolean moveAsked) {
    this.moveAsked = moveAsked;
  }
}
