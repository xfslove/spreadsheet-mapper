package spread.sheet.w2o.validator;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.*;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.model.meta.SheetMetaBean;
import spread.sheet.w2o.validator.cell.CellValidator;
import spread.sheet.w2o.validator.cell.CellValidatorAdapter;
import spread.sheet.w2o.validator.row.RowValidator;
import spread.sheet.w2o.validator.row.RowValidatorAdapter;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetValidateEngineTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultSheetValidateEngineTest.class);

  @Test(groups = "missingTest")
  public void testMissing() {

    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"3"}),
        new TestCellValidator("2", new String[]{})
    };

    SheetValidateEngine sheetValidateEngine = new DefaultSheetValidateEngine().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());
    sheetValidateEngine.cellValidator(cellValidators);

    boolean missing = false;
    try {
      sheetValidateEngine.valid();
    } catch (WorkbookValidateException e) {
      LOGGER.debug(ExceptionUtils.getStackTrace(e));
      if (StringUtils.contains(e.getMessage(), "missing")) {
        missing = true;
      }
    }

    assertTrue(missing);
  }

  @Test(groups = "cyclingTest")
  public void testCycling0() {

    /*
        cycle
        1 -> 2
        2 -> 2
     */
    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"2"}),
        new TestCellValidator("2", new String[]{})
    };

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator("2", new String[]{"2"})
    };
    assertValid(cellValidators, rowValidators, true);
  }

  @Test(groups = "cyclingTest")
  public void testCycling1() {

    /*
       no cycle
       1 -> 2, 3, 7
       2 -> 4
       3 -> 6
       4 -> 5, 6
       5 -> 7
       6 -> 7
     */
    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"2"}),
        new TestCellValidator("1", new String[]{"3"}),
        new TestCellValidator("1", new String[]{}),
        new TestCellValidator("1", new String[]{}),
        new TestCellValidator("2", new String[]{"4"}),
        new TestCellValidator("3", new String[]{"6"}),
        new TestCellValidator("3", new String[]{}),
        new TestCellValidator("3", new String[]{}),
        new TestCellValidator("3", new String[]{}),
        new TestCellValidator("4", new String[]{"5", "6"}),
        new TestCellValidator("5", new String[]{"7"}),
        new TestCellValidator("6", new String[]{}),
        new TestCellValidator("6", new String[]{"7"}),
        new TestCellValidator("6", new String[]{}),
        new TestCellValidator("7", new String[]{})
    };

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator("1", new String[]{"7"})
    };

    assertValid(cellValidators, rowValidators, false);
  }

  @Test(groups = "cyclingTest")
  public void testCycling2() {

    /*
       cycle
       1 -> 2
       2 -> 4
       3 -> 4,5
       4 -> 6, 1
       5 -> 6
     */
    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"2"}),
        new TestCellValidator("2", new String[]{"4"}),
        new TestCellValidator("3", new String[]{"3", "4"}),
        new TestCellValidator("5", new String[]{"6"}),
        new TestCellValidator("6", new String[0])
    };

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator("4", new String[]{"6", "1"}),
    };

    assertValid(cellValidators, rowValidators, true);
  }

  @Test(groups = "cyclingTest")
  public void testCycling3() {

    /*
      no cycle
      1 -> 2
      2 -> 3,7
      3 -> 4,5
      4 -> 8
      5 -> 6
      6 -> 8
      7 -> 5,9,11
      8 -> 9
      9 -> 10,11
      10 -> 12
      11 -> 10
     */

    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"2"}),
        new TestCellValidator("2", new String[]{"3", "7"}),
        new TestCellValidator("3", new String[]{"4", "5"}),
        new TestCellValidator("4", new String[]{"8"}),
        new TestCellValidator("5", new String[]{"6"}),
        new TestCellValidator("6", new String[]{"8"}),
        new TestCellValidator("7", new String[]{"5", "9", "11"}),
        new TestCellValidator("8", new String[]{"9"}),
        new TestCellValidator("9", new String[]{"10", "11"}),
        new TestCellValidator("10", new String[]{"12"}),
        new TestCellValidator("11", new String[]{"10"}),
        new TestCellValidator("12", new String[0]),
    };

    assertValid(cellValidators, null, false);
  }

  @Test(groups = "cyclingTest")
  public void testCycling4() {

    /*
      cycle
      1 -> 2
      2 -> 3,7
      3 -> 4,5
      4 -> 8
      5 -> 6
      6 -> 8
      7 -> 5,9,11
      8 -> 9
      9 -> 10,11
      10 -> 12
      11 -> 10,6
     */

    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"2"}),
        new TestCellValidator("2", new String[]{"3", "7"}),
        new TestCellValidator("3", new String[]{"4", "5"}),
        new TestCellValidator("4", new String[]{"8"}),
        new TestCellValidator("5", new String[]{"6"}),
        new TestCellValidator("6", new String[]{"8"}),
        new TestCellValidator("7", new String[]{"5", "9", "11"}),
        new TestCellValidator("8", new String[]{"9"}),
        new TestCellValidator("9", new String[]{"10", "11"}),
        new TestCellValidator("10", new String[]{"12"}),
        new TestCellValidator("11", new String[]{"10", "6"}),
        new TestCellValidator("12", new String[0]),
    };

    assertValid(cellValidators, null, true);
  }

  @Test(dependsOnGroups = {"cyclingTest", "missingTest"})
  public void validatorHitTest() {

    Counter counter = new Counter();

    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("test.int1", new String[]{"test.int2"}, counter),
        new TestCellValidator("test.int2", new String[]{"test.long1"}, counter),
        new TestCellValidator("test.long1", new String[]{"test.long2"}, counter),
        new TestCellValidator("test.long2", new String[]{"test.double2"}, counter),
        new TestCellValidator("test.float1", new String[]{"test.float2"}, counter),
        new TestCellValidator("test.float2", new String[]{"test.double2"}, counter),
        new TestCellValidator("test.double1", new String[]{"test.float1"}, counter),
        new TestCellValidator("test.double2", new String[]{"test.string"}, counter),
        new TestCellValidator("test.string", new String[]{"test.boolean1"}, counter),
        new TestCellValidator("test.string", new String[]{"test.boolean2"}, counter),
        new TestCellValidator("test.boolean1", new String[]{"test.bigDecimal"}, counter),
        new TestCellValidator("test.boolean2", new String[]{"test.boolean1"}, counter),
        new TestCellValidator("test.bigDecimal", new String[0], counter),
    };

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator("test.int2", new String[]{"test.double1"}, counter),
        new TestRowValidator("test.long1", new String[]{"test.float1"}, counter),
        new TestRowValidator("test.double1", new String[]{"test.string"}, counter),
        new TestRowValidator("test.double1", new String[]{"test.boolean2"}, counter),
        new TestRowValidator("test.boolean2", new String[]{"test.boolean1"}, counter),
    };

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = TestFactory.createSheet();

    SheetValidateEngine sheetValidateEngine = new DefaultSheetValidateEngine().sheetMeta(sheetMeta).sheet(sheet);
    sheetValidateEngine.cellValidator(cellValidators);
    sheetValidateEngine.rowValidator(rowValidators);

    boolean valid = sheetValidateEngine.valid();
    assertTrue(valid);
    assertEquals(counter.hitTime(), 18);
  }

  private void assertValid(CellValidator[] cellValidators, RowValidator[] rowValidators, boolean cycling) {
    SheetValidateEngine sheetValidateEngine = new DefaultSheetValidateEngine().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());

    sheetValidateEngine.cellValidator(cellValidators);
    sheetValidateEngine.rowValidator(rowValidators);

    boolean result = false;
    try {
      sheetValidateEngine.valid();
    } catch (WorkbookValidateException e) {
      LOGGER.debug(ExceptionUtils.getStackTrace(e));
      if (StringUtils.contains(e.getMessage(), "cycling")) {
        result = true;
      }
    }
    if (cycling) {
      assertTrue(result);
    } else {
      assertFalse(result);
    }
  }

  private class TestCellValidator extends CellValidatorAdapter {

    private Counter counter;

    TestCellValidator(String matchField, String[] dependsOn, Counter counter) {
      super(matchField, "", dependsOn);
      this.counter = counter;
    }

    TestCellValidator(String matchField, String[] dependsOn) {
      super(matchField, "", dependsOn);
    }

    @Override
    protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
      counter.hit();
      return true;
    }
  }

  private class TestRowValidator extends RowValidatorAdapter {

    private Counter counter;

    public TestRowValidator(String group, String[] dependsOn, Counter counter) {
      super(group, "", new String[0], dependsOn);
      this.counter = counter;
    }

    public TestRowValidator(String group, String[] dependsOn) {
      super(group, "", new String[0], dependsOn);
    }

    @Override
    protected boolean customValidate(Row row, SheetMeta sheetMeta) {
      counter.hit();
      return true;
    }
  }

  private class Counter {
    private int count = 1;

    void hit() {
      count++;
    }

    int hitTime() {
      return count;
    }
  }

  private Sheet getSheet() {
    Sheet sheet = TestFactory.createSheet();
    Map<String, Cell> errorCellMap = TestFactory.createErrorCellMap();
    Row er = new RowBean();
    for (Cell cell : errorCellMap.values()) {
      er.addCell(cell);
    }
    sheet.addRow(er);
    return sheet;
  }
}