package view;

import controller.GameChoiceListener;
import controller.GraphicsMouseListener;
import controller.RoleSelectListener;
import controller.StartListener;
import enums.Game;
import enums.Player;
import helpers.PredicateHelper;
import model.GameModel;

import java.awt.Font;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButtonMenuItem;

public class ManagerView extends JFrame implements Observer {
  private static final long serialVersionUID = 1L;

  private final GameModel currentModel;
  private final GameCanvas gameCanvas;
  private GraphicsMouseListener gmListener;
  private JLabel lblControl;
  private JLabel lblStatus;
  private final JMenu mnPlayers;

  /**
   * Create the view.
   */
  public ManagerView(final GameModel model) {
    this.setTitle("General Game Playing");
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    final JMenuBar menuBar = new JMenuBar();
    this.setJMenuBar(menuBar);

    final JMenu mnGame = new JMenu("Game");
    menuBar.add(mnGame);

    // Add menu entries for games
    for (final Game g : Game.values()) {
      final JRadioButtonMenuItem gameOption = new JRadioButtonMenuItem(g.getGameName());
      mnGame.add(gameOption);
      // Attach listener
      gameOption.addActionListener(new GameChoiceListener(model));
    }

    mnPlayers = new JMenu("Players");
    menuBar.add(mnPlayers);

    JMenuItem warning = new JMenuItem("LOAD A GAME");
    mnPlayers.add(warning);

    // Dimensions offset by 1 for friendlier calculations later
    int canvasDim = 421;

    // Create canvas for drawing game state
    gameCanvas = new GameCanvas(canvasDim, canvasDim);
    this.getContentPane().add(gameCanvas);

    gmListener = new GraphicsMouseListener(model);
    gameCanvas.addMouseListener(gmListener);

    getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    JPanel infopanel = new JPanel();
    getContentPane().add(infopanel);
    infopanel.setLayout(new GridLayout(0, 3, 50, 0));

    final JButton btnStart = new JButton("Start");
    infopanel.add(btnStart);
    btnStart.setFont(new Font("Segoe UI", Font.PLAIN, 12));
    btnStart.addActionListener(new StartListener(model));

    lblStatus = new JLabel("");
    infopanel.add(lblStatus);

    lblControl = new JLabel("");
    infopanel.add(lblControl);

    this.pack();
    setResizable(false);
    this.setVisible(true);

    // Attach to model to receive updates
    model.addObserver(this);
    currentModel = model;
  }

  // Get updates from model
  @Override
  public void update(final Observable model, final Object arg) {
    // Re-initialisations
    lblStatus.setText(null);

    switch (arg.toString()) {
      case "roles":
        // Update player choices in menu
        mnPlayers.removeAll();
        for (String role : currentModel.getRoles()) {
          JMenu roleMenu = new JMenu(role);
          mnPlayers.add(roleMenu);

          for (Player p : Player.values()) {
            JRadioButtonMenuItem playerItem = new JRadioButtonMenuItem(role + ":" + p.toString());
            roleMenu.add(playerItem);
            // Attach listener
            playerItem.addActionListener(new RoleSelectListener(currentModel));
          }
        }
        break;
      case "state":
        // Tell game canvas to draw state
        // Helper to convert state predicates into array of cell contents
        ArrayList<String> preds = (ArrayList<String>) currentModel.getStatePredicates();
        String[][] cells = PredicateHelper.predicatesToCells(currentModel.getStatePredicates());
        gameCanvas.setGridContents(cells);

        lblControl.setText(PredicateHelper.getControl(preds));
        break;
      case "make_move":
        gmListener.setMoveAsked(true);
        lblStatus.setText("CLICK TO ENTER MOVE");
        break;
      case "terminal":
        String mes = "Game has now finished.";
        JOptionPane.showMessageDialog(null, mes, "Game Over", JOptionPane.INFORMATION_MESSAGE);
        break;
      default:
        throw new IllegalArgumentException();
    }
  }
}
