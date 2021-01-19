package enums;

public enum Game {
  XO("xo.pl", "XOs");

  /**
   * Convert from game name to enum.
   * 
   * @param name
   *          String to be converted.
   * @return If provided name is an enum instance, return enum. Otherwise,
   *         return null.
   */
  public static Game fromName(String name) {
    for (Game g : Game.values()) {
      if (g.getGameName().equals(name)) {
        return g;
      }
    }
    return null;
  }

  private String fileLocation;

  private String gameName;

  Game(String fileL, String gameN) {
    this.fileLocation = "logic/gamedescription/" + fileL;
    this.gameName = gameN;
  }

  public String getFileLocation() {
    return fileLocation;
  }

  public String getGameName() {
    return gameName;
  }

}
