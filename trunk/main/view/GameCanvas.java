package view;

import java.awt.Canvas;
import java.awt.Graphics;

public class GameCanvas extends Canvas {
  private static final long serialVersionUID = 1L;

  private String[][] gridContents;

  /**
   * Basic constructor.
   * 
   * @param width   Width of new canvas.
   * @param height  Height of new canvas.
   */
  public GameCanvas(int width, int height) {
    setSize(width, height);
    gridContents = null;
  }

  /**
   * Draw the game state.
   * 
   * @param gr
   *          Graphics
   */
  @Override
  public void paint(Graphics gr) {
    int maxX = this.getWidth() - 1;
    int maxY = this.getHeight() - 1;

    // Draw grid
    for (int i = 0; i < 4; i++) {
      gr.drawLine(0, maxY * i / 3, maxX, maxY * i / 3);
      gr.drawLine(maxX * i / 3, 0, maxX * i / 3, maxY);
    }

    // Draw grid contents if possible
    if (gridContents != null) {
      for (int x = 0; x < 3; x++) {
        for (int y = 0; y < 3; y++) {
          int pointX = x * maxX / 3;
          int pointY = y * maxX / 3;
          int length = maxX / 3;
          if (gridContents[x][y].equals("o")) {
            gr.drawOval(pointX, pointY, length, length);
          } else if (gridContents[x][y].equals("x")) {
            gr.drawLine(pointX, pointY, pointX + length, pointY + length);
            gr.drawLine(pointX + length, pointY, pointX, pointY + length);
          }
        }
      }
    }
  }

  public void setGridContents(String[][] newContents) {
    gridContents = newContents;
    repaint();
  }

}
