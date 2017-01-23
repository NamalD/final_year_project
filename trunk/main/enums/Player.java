package enums;

public enum Player {
  Human(null), Random("random.pl");

  /**
   * Create new enum instance from some String.
   * @param str String to attempt to covert.
   * @return    New enum instance if provided String is correct, null object otherwise.
   */
  public static Player fromString(String str) {
    for (Player p : Player.values()) {
      if (p.name().toLowerCase().equals(str.toLowerCase())) {
        return p;
      }
    }
    return null;
  }

  private String fileLocation;

  Player(String fileLocation) {
    if (fileLocation != null) {
      this.fileLocation = "logic/decisionmaking/" + fileLocation;
    } else {
      this.fileLocation = null;
    }
  }

  public String getFileLocation() {
    return fileLocation;
  }
}
