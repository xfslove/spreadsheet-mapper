package spreadsheet.mapper.w2o.validator;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.core.SheetBean;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.meta.SheetMetaBean;
import spreadsheet.mapper.w2o.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validator.cell.CellValidatorAdapter;
import spreadsheet.mapper.w2o.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validator.row.RowValidatorAdapter;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetValidateHelperTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultSheetValidateHelperTest.class);

  @Test(groups = "missingTest")
  public void testMissing() {

    CellValidator[] cellValidators = new CellValidator[]{
        new TestCellValidator("1", new String[]{"3"}),
        new TestCellValidator("2", new String[]{})
    };

    SheetValidateHelper sheetValidateHelper = new DefaultSheetValidateHelper().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());
    sheetValidateHelper.cellValidator(cellValidators);

    boolean missing = false;
    try {
      sheetValidateHelper.valid();
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

  @Test(groups = "hitTest", dependsOnGroups = {"cyclingTest", "missingTest"})
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
    Sheet sheet = getSheet();

    SheetValidateHelper sheetValidateHelper = new DefaultSheetValidateHelper().sheetMeta(sheetMeta).sheet(sheet);
    sheetValidateHelper.cellValidator(cellValidators);
    sheetValidateHelper.rowValidator(rowValidators);

    boolean valid = sheetValidateHelper.valid();
    assertTrue(valid);
    assertEquals(counter.hitTime(), 13 + 5);
  }

  @Test(dependsOnGroups = "hitTest")
  public void testSkip() {

    Set<String> hitValidators = new HashSet<>();
    /*
       no cycle
       test.int1 -> test.int2, test.long1, test.double1
       test.int2 -> test.long2
       test.long1 -> test.float2
       test.long2 -> test.float1, test.float2
       test.float1 -> test.double1
       test.float2 -> test.double1
     */
    CellValidator[] cellValidators = new CellValidator[]{
        new TrueCellValidator("test.float2", new String[]{"test.double1"}, hitValidators),
        new FalseCellValidator("test.int1", new String[]{"test.int2"}, hitValidators),
        new TrueCellValidator("test.int1", new String[]{"test.long1"}, hitValidators),
        new TrueCellValidator("test.int2", new String[]{"test.long2"}, hitValidators),
        new TrueCellValidator("test.long1", new String[]{"test.float2"}, hitValidators),
        new TrueCellValidator("test.long2", new String[]{"test.float1"}, hitValidators),
        new FalseCellValidator("test.float1", new String[0], hitValidators),
        new TrueCellValidator("test.double1", new String[0], hitValidators)
    };
    RowValidator[] rowValidators = new RowValidator[]{
        new TrueRowValidator("test.int1", new String[]{"test.double1"}, hitValidators),
        new TrueRowValidator("test.long2", new String[]{"test.float2"}, hitValidators),
        new FalseRowValidator("test.float1", new String[]{"test.double1"}, hitValidators)
    };

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = getSheet();

    SheetValidateHelper sheetValidateHelper = new DefaultSheetValidateHelper().sheetMeta(sheetMeta).sheet(sheet);
    sheetValidateHelper.cellValidator(cellValidators);
    sheetValidateHelper.rowValidator(rowValidators);

    sheetValidateHelper.valid();

    Set<String> expected = new HashSet<>(Arrays.asList("cell:true:test.float2", "cell:true:test.double1", "row:false:test.float1"));

    assertEquals(hitValidators, expected);
  }

  private void assertValid(CellValidator[] cellValidators, RowValidator[] rowValidators, boolean cycling) {
    SheetValidateHelper sheetValidateHelper = new DefaultSheetValidateHelper().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());

    sheetValidateHelper.cellValidator(cellValidators);
    sheetValidateHelper.rowValidator(rowValidators);

    boolean result = false;
    try {
      sheetValidateHelper.valid();
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

  private class TrueCellValidator extends CellValidatorAdapter {

    private Set<String> hitValidators;

    TrueCellValidator(String matchField, String[] dependsOn, Set<String> hitValidators) {
      super(matchField, "", dependsOn);
      this.hitValidators = hitValidators;
    }

    @Override
    protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
      hitValidators.add("cell:true:" + getGroup());
      return true;
    }

  }

  private class TrueRowValidator extends RowValidatorAdapter {

    private Set<String> hitValidators;

    TrueRowValidator(String group, String[] dependsOn, Set<String> hitValidators) {
      super(group, "", new String[0], dependsOn);
      this.hitValidators = hitValidators;
    }

    @Override
    protected boolean customValidate(Row row, SheetMeta sheetMeta) {
      hitValidators.add("row:true:" + getGroup());
      return false;
    }
  }

  private class FalseCellValidator extends CellValidatorAdapter {

    private Set<String> hitValidators;

    FalseCellValidator(String matchField, String[] dependsOn, Set<String> hitValidators) {
      super(matchField, "", dependsOn);
      this.hitValidators = hitValidators;
    }

    @Override
    protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
      hitValidators.add("cell:false:" + getGroup());
      return false;
    }
  }

  private class FalseRowValidator extends RowValidatorAdapter {

    private Set<String> hitValidators;

    FalseRowValidator(String group, String[] dependsOn, Set<String> hitValidators) {
      super(group, "", new String[0], dependsOn);
      this.hitValidators = hitValidators;
    }

    @Override
    protected boolean customValidate(Row row, SheetMeta sheetMeta) {
      hitValidators.add("row:false:" + getGroup());
      return false;
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
      if (counter != null) {
        counter.hit();
      }
      return true;
    }
  }

  private class TestRowValidator extends RowValidatorAdapter {

    private Counter counter;

    TestRowValidator(String group, String[] dependsOn, Counter counter) {
      super(group, "", new String[0], dependsOn);
      this.counter = counter;
    }

    TestRowValidator(String group, String[] dependsOn) {
      super(group, "", new String[0], dependsOn);
    }

    @Override
    protected boolean customValidate(Row row, SheetMeta sheetMeta) {
      if (counter != null) {
        counter.hit();
      }
      return true;
    }
  }

  private class Counter {
    private int count = 0;

    void hit() {
      count++;
    }

    int hitTime() {
      return count;
    }
  }

  private Sheet getSheet() {
    Sheet baseSheet = TestFactory.createSheet();
    Sheet sheet = new SheetBean();
    sheet.addRow(baseSheet.getRow(1));
    sheet.addRow(baseSheet.getRow(2));
    return sheet;
  }
}