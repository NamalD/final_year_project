// Check integration is functioning

package tests;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Before;
import org.junit.Test;

import helpers.LogicConnector;

public class ConnectorTest {
  LogicConnector testLoader;
  File tPFile, xoFile, manFile;

  @Before
  public void setUp() throws Exception {
    testLoader = new LogicConnector();
    tPFile = new File("logic/temporalPredicates.pl");
    xoFile = new File("logic/gamedescription/xo.pl");
    manFile = new File("logic/manager.pl");
  }

  // Load temporal predicates through constructor
  @Test
  public void testLoadConstructor() {
    testLoader = new LogicConnector(tPFile);
    assertTrue(testLoader.predicateExists("holds_at/2"));
  }

  // Load XO game definition through method
  @Test
  public void testLoadMethod() {
    testLoader.consultFile(xoFile);
    assertTrue(testLoader.predicateExists("init/1"));
  }

  // Load multiple files through constructor
  @Test
  public void testLoadMultipleConstructor() {
    File[] loadFiles = new File[] { tPFile, xoFile, manFile };
    testLoader = new LogicConnector(loadFiles);
    assertTrue(testLoader.predicateExists("get_state/2"));
  }

}
