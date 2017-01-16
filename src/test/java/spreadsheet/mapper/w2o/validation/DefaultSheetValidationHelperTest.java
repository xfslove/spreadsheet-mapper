package spreadsheet.mapper.w2o.validation;

import org.apache.commons.lang3.StringUtils;
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
import spreadsheet.mapper.w2o.validation.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.CellValidatorAdapter;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidatorAdapter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetValidationHelperTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultSheetValidationHelperTest.class);

  @Test(groups = "missingTest")
  public void testMissing() {

    CellValidator cellValidators1 = new TestCellValidator().matchField("1").dependsOn("3").end();
    CellValidator cellValidators2 = new TestCellValidator().matchField("2").end();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());
    sheetValidationHelper.cellValidators(cellValidators1);
    sheetValidationHelper.cellValidators(cellValidators2);

    boolean missing = false;
    try {
      sheetValidationHelper.valid();
    } catch (WorkbookValidateException e) {
      LOGGER.debug("valid missing dependency group");
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
        3 -> 4
        4 -> 4
     */
    List<CellValidator> cellValidators = new ArrayList<>();

    Collections.addAll(cellValidators, new TestCellValidator().group("3").matchField("1").dependsOn("4").messageOnField("5").errorMessage("test1").end());
    Collections.addAll(cellValidators, new TestCellValidator().group("4").matchField("2").messageOnField("6").errorMessage("test2").end());

    assertEquals(cellValidators.get(0).getMatchField(), "1");
    assertEquals(cellValidators.get(1).getMatchField(), "2");
    assertEquals(cellValidators.get(0).getGroup(), "3");
    assertEquals(cellValidators.get(1).getGroup(), "4");
    assertEquals(cellValidators.get(0).getErrorMessage(), "test1");
    assertEquals(cellValidators.get(1).getErrorMessage(), "test2");
    assertEquals(cellValidators.get(0).getMessageOnField(), "5");
    assertEquals(cellValidators.get(1).getMessageOnField(), "6");

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator().group("4").dependsOn("4").messageOnFields("2").errorMessage("test2").end()
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
    List<CellValidator> cellValidators = new ArrayList<>();
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("3").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("4").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("5", "6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("7").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("7").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7").end());

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator().group("1").dependsOn("7").end()
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
    List<CellValidator> cellValidators = new ArrayList<>();

    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("4").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").end());

    RowValidator[] rowValidators = new RowValidator[]{
        new TestRowValidator().group("4").dependsOn("6", "1").end(),
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
    List<CellValidator> cellValidators = new ArrayList<>();
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("3", "7").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("8").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("8").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7").dependsOn("5", "9", "11").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("8").dependsOn("9").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("9").dependsOn("10", "11").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("10").dependsOn("12").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("11").dependsOn("10").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("12").end());

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
    List<CellValidator> cellValidators = new ArrayList<>();
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("3", "7").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("8").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("8").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7").dependsOn("5", "9", "11").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("8").dependsOn("9").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("9").dependsOn("10", "11").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("10").dependsOn("12").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("11").dependsOn("10", "6").end());
    Collections.addAll(cellValidators, new TestCellValidator().matchField("12").end());

    assertValid(cellValidators, null, true);
  }

  @Test(groups = "hitTest", dependsOnGroups = {"cyclingTest", "missingTest"})
  public void validatorHitTest() {

    Counter counter = new Counter();

    List<CellValidator> cellValidators = new ArrayList<>();
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.int1").dependsOn("test.int2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.int2").dependsOn("test.long1").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.long1").dependsOn("test.long2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.long2").dependsOn("test.double2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.float1").dependsOn("test.float2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.float2").dependsOn("test.double2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.double1").dependsOn("test.float1").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.double2").dependsOn("test.string").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.string").dependsOn("test.boolean1").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.string").dependsOn("test.boolean2").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.boolean1").dependsOn("test.bigDecimal").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.boolean2").dependsOn("test.boolean1").end());
    Collections.addAll(cellValidators, new TestCellValidator(counter).matchField("test.bigDecimal").end());

    List<RowValidator> rowValidators = new ArrayList<>();
    rowValidators.add(new TestRowValidator(counter).group("test.int2").dependsOn("test.double1").end());
    rowValidators.add(new TestRowValidator(counter).group("test.long1").dependsOn("test.float1").end());
    rowValidators.add(new TestRowValidator(counter).group("test.double1").dependsOn("test.string").end());
    rowValidators.add(new TestRowValidator(counter).group("test.double1").dependsOn("test.boolean2").end());
    rowValidators.add(new TestRowValidator(counter).group("test.boolean2").dependsOn("test.boolean1").end());

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = getSheet();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper().sheetMeta(sheetMeta).sheet(sheet);
    sheetValidationHelper.cellValidators(cellValidators.toArray(new CellValidator[0]));
    sheetValidationHelper.rowValidators(rowValidators.toArray(new RowValidator[0]));

    boolean valid = sheetValidationHelper.valid();
    assertTrue(valid);
    assertEquals(counter.hitTime(), 13 + 5);
  }

  @Test(dependsOnGroups = "hitTest")
  public void testSkip() {

    List<String> hitValidators = new ArrayList<>();
    /*
       no cycle
       test.int1 -> test.int2, test.long1, test.double1
       test.int2 -> test.long2
       test.long1 -> test.float2
       test.long2 -> test.float1, test.float2
       test.float1 -> test.double1
       test.float2 -> test.double1
     */
    List<CellValidator> cellValidators = new ArrayList<>();
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.float2").dependsOn("test.double1").end());
    Collections.addAll(cellValidators, new FalseCellValidator(hitValidators).matchField("test.int1").dependsOn("test.int2").end());
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.int1").dependsOn("test.long1").end());
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.int2").dependsOn("test.long2").end());
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.long1").dependsOn("test.float2").end());
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.long2").dependsOn("test.float1").end());
    Collections.addAll(cellValidators, new FalseCellValidator(hitValidators).matchField("test.float1").end());
    Collections.addAll(cellValidators, new TrueCellValidator(hitValidators).matchField("test.double1").end());

    List<RowValidator> rowValidators = new ArrayList<>();
    rowValidators.add(new TrueRowValidator(hitValidators).group("test.int1").dependsOn("test.double1").end());
    rowValidators.add(new TrueRowValidator(hitValidators).group("test.long2").dependsOn("test.float2").end());
    rowValidators.add(new FalseRowValidator(hitValidators).group("test.float1").dependsOn("test.double1").end());

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = getSheet();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper().sheetMeta(sheetMeta).sheet(sheet);
    sheetValidationHelper.cellValidators(cellValidators.toArray(new CellValidator[0]));
    sheetValidationHelper.rowValidators(rowValidators.toArray(new RowValidator[0]));

    boolean result = sheetValidationHelper.valid();
    assertFalse(result);

    List<String> expected = Arrays.asList("cell:true:test.double1", "cell:true:test.float2", "row:false:test.float1");

    assertEquals(hitValidators, expected);
  }

  private void assertValid(List<CellValidator> cellValidators, RowValidator[] rowValidators, boolean cycling) {
    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());

    sheetValidationHelper.cellValidators(cellValidators.toArray(new CellValidator[0]));
    sheetValidationHelper.rowValidators(rowValidators);

    boolean result = false;
    try {
      sheetValidationHelper.valid();
    } catch (WorkbookValidateException e) {
      LOGGER.debug("valid dependency cycling");
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

    private List<String> hitValidators;

    private String group;

    TrueCellValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public CellValidatorAdapter matchField(String matchField) {
      this.group = matchField;
      return super.matchField(matchField);
    }

    @Override
    protected CellValidatorAdapter getThis() {
      return this;
    }

    @Override
    protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
      hitValidators.add("cell:true:" + group);
      return true;
    }

  }

  private class TrueRowValidator extends RowValidatorAdapter {

    private List<String> hitValidators;

    private String group;

    TrueRowValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public RowValidatorAdapter group(String group) {
      this.group = group;
      return super.group(group);
    }

    @Override
    protected boolean customValid(Row row, SheetMeta sheetMeta) {
      hitValidators.add("row:true:" + group);
      return false;
    }

    @Override
    protected RowValidatorAdapter getThis() {
      return this;
    }
  }

  private class FalseCellValidator extends CellValidatorAdapter {

    private List<String> hitValidators;

    private String group;

    FalseCellValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public CellValidatorAdapter matchField(String matchField) {
      this.group = matchField;
      return super.matchField(matchField);
    }

    @Override
    protected CellValidatorAdapter getThis() {
      return this;
    }

    @Override
    protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
      hitValidators.add("cell:false:" + group);
      return false;
    }
  }

  private class FalseRowValidator extends RowValidatorAdapter {

    private List<String> hitValidators;

    private String group;

    FalseRowValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public RowValidatorAdapter group(String group) {
      this.group = group;
      return super.group(group);
    }

    @Override
    protected boolean customValid(Row row, SheetMeta sheetMeta) {
      hitValidators.add("row:false:" + group);
      return false;
    }

    @Override
    protected RowValidatorAdapter getThis() {
      return this;
    }
  }

  private class TestCellValidator extends CellValidatorAdapter {

    private Counter counter;

    TestCellValidator() {
    }

    TestCellValidator(Counter counter) {
      this.counter = counter;
    }

    @Override
    protected CellValidatorAdapter getThis() {
      return this;
    }

    @Override
    protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
      if (counter != null) {
        counter.hit();
      }
      return true;
    }
  }

  private class TestRowValidator extends RowValidatorAdapter {

    private Counter counter;

    TestRowValidator(Counter counter) {
      this.counter = counter;
    }

    TestRowValidator() {
    }

    @Override
    protected boolean customValid(Row row, SheetMeta sheetMeta) {
      if (counter != null) {
        counter.hit();
      }
      return true;
    }

    @Override
    protected RowValidatorAdapter getThis() {
      return this;
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