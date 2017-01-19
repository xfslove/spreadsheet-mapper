package spreadsheet.mapper.w2o.validation;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
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
@Test(groups = "sheetValidationHelperTest")
public class DefaultSheetValidationHelperTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultSheetValidationHelperTest.class);

  @BeforeClass
  public void before() {
    LOGGER.debug("-------------------starting test sheet validation helper-------------------");
  }

  @Test(groups = "missingTest")
  public void testMissing() {

    CellValidator cellValidators1 = new TestCellValidator().matchField("1").dependsOn("3");
    CellValidator cellValidators2 = new TestCellValidator().matchField("2");

    SheetMetaBean sheetMetaBean = new SheetMetaBean(2);
    SheetBean sheetBean = new SheetBean();
    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper();
    sheetValidationHelper.addCellValidator(cellValidators1);
    sheetValidationHelper.addCellValidator(cellValidators2);

    boolean missing = false;
    try {
      sheetValidationHelper.valid(sheetBean, sheetMetaBean);
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

    Collections.addAll(cellValidators, new TestCellValidator().group("3").matchField("1").dependsOn("4").messageOnField("5").errorMessage("test1"));
    Collections.addAll(cellValidators, new TestCellValidator().group("4").matchField("2").messageOnField("6").errorMessage("test2"));

    assertEquals(cellValidators.get(0).getMatchField(), "1");
    assertEquals(cellValidators.get(1).getMatchField(), "2");
    assertEquals(cellValidators.get(0).getGroup(), "3");
    assertEquals(cellValidators.get(1).getGroup(), "4");
    assertEquals(cellValidators.get(0).getErrorMessage(), "test1");
    assertEquals(cellValidators.get(1).getErrorMessage(), "test2");
    assertEquals(cellValidators.get(0).getMessageOnField(), "5");
    assertEquals(cellValidators.get(1).getMessageOnField(), "6");

    List<RowValidator> rowValidators = new ArrayList<>();
    rowValidators.add(new TestRowValidator().group("4").dependsOn("4").messageOnFields("2").errorMessage("test2"));

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
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("3"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("4"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("5", "6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("7"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("7"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7"));

    List<RowValidator> rowValidators = new ArrayList<>();
    rowValidators.add(new TestRowValidator().group("1").dependsOn("7"));

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

    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("4"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6"));

    List<RowValidator> rowValidators = new ArrayList<>();
    rowValidators.add(new TestRowValidator().group("4").dependsOn("6", "1"));

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
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("3", "7"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("8"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("8"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7").dependsOn("5", "9", "11"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("8").dependsOn("9"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("9").dependsOn("10", "11"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("10").dependsOn("12"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("11").dependsOn("10"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("12"));

    assertValid(cellValidators, Collections.<RowValidator>emptyList(), false);
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
    Collections.addAll(cellValidators, new TestCellValidator().matchField("1").dependsOn("2"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("2").dependsOn("3", "7"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("3").dependsOn("4", "5"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("4").dependsOn("8"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("5").dependsOn("6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("6").dependsOn("8"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("7").dependsOn("5", "9", "11"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("8").dependsOn("9"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("9").dependsOn("10", "11"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("10").dependsOn("12"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("11").dependsOn("10", "6"));
    Collections.addAll(cellValidators, new TestCellValidator().matchField("12"));

    assertValid(cellValidators, Collections.<RowValidator>emptyList(), true);
  }

  @Test(groups = "hitTest", dependsOnGroups = {"cyclingTest", "missingTest"})
  public void validatorHitTest() {

    Counter counter = new Counter();

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = getSheet();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper();

    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("int1").dependsOn("int2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("int2").dependsOn("long1"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("long1").dependsOn("long2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("long2").dependsOn("double2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("float1").dependsOn("float2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("float2").dependsOn("double2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("double1").dependsOn("float1"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("double2").dependsOn("string"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("string").dependsOn("boolean1"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("string").dependsOn("boolean2"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("boolean1").dependsOn("bigDecimal"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("boolean2").dependsOn("boolean1"));
    sheetValidationHelper.addCellValidator(new TestCellValidator(counter).matchField("bigDecimal"));

    sheetValidationHelper.addRowValidator(new TestRowValidator(counter).group("int2").dependsOn("double1"));
    sheetValidationHelper.addRowValidator(new TestRowValidator(counter).group("long1").dependsOn("float1"));
    sheetValidationHelper.addRowValidator(new TestRowValidator(counter).group("double1").dependsOn("string"));
    sheetValidationHelper.addRowValidator(new TestRowValidator(counter).group("double1").dependsOn("boolean2"));
    sheetValidationHelper.addRowValidator(new TestRowValidator(counter).group("boolean2").dependsOn("boolean1"));

    boolean valid = sheetValidationHelper.valid(sheet, sheetMeta);
    assertTrue(valid);
    assertEquals(counter.hitTime(), 13 + 5);
  }

  @Test(dependsOnGroups = "hitTest")
  public void testSkip() {

    List<String> hitValidators = new ArrayList<>();
    /*
       no cycle
       int1 -> int2, long1, double1
       int2 -> long2
       long1 -> float2
       long2 -> float1, float2
       float1 -> double1
       float2 -> double1
     */
    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);
    Sheet sheet = getSheet();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper();
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("float2").dependsOn("double1"));
    sheetValidationHelper.addCellValidator(new FalseCellValidator(hitValidators).matchField("int1").dependsOn("int2"));
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("int1").dependsOn("long1"));
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("int2").dependsOn("long2"));
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("long1").dependsOn("float2"));
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("long2").dependsOn("float1"));
    sheetValidationHelper.addCellValidator(new FalseCellValidator(hitValidators).matchField("float1"));
    sheetValidationHelper.addCellValidator(new TrueCellValidator(hitValidators).matchField("double1"));

    sheetValidationHelper.addRowValidator(new TrueRowValidator(hitValidators).group("int1").dependsOn("double1"));
    sheetValidationHelper.addRowValidator(new TrueRowValidator(hitValidators).group("long2").dependsOn("float2"));
    sheetValidationHelper.addRowValidator(new FalseRowValidator(hitValidators).group("float1").dependsOn("double1"));
    boolean result = sheetValidationHelper.valid(sheet, sheetMeta);
    assertFalse(result);

    List<String> expected = Arrays.asList("cell:true:double1", "cell:true:float2", "row:false:float1");

    assertEquals(hitValidators, expected);
  }

  private void assertValid(List<CellValidator> cellValidators, List<RowValidator> rowValidators, boolean cycling) {
    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper();

    for (RowValidator rowValidator : rowValidators) {
      sheetValidationHelper.addRowValidator(rowValidator);
    }

    for (CellValidator cellValidator : cellValidators) {
      sheetValidationHelper.addCellValidator(cellValidator);
    }

    boolean result = false;
    try {
      sheetValidationHelper.valid(new SheetBean(), new SheetMetaBean(2));
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

  private class TrueCellValidator extends CellValidatorAdapter<TrueCellValidator> {

    private List<String> hitValidators;

    private String group;

    TrueCellValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public TrueCellValidator matchField(String matchField) {
      this.group = matchField;
      return super.matchField(matchField);
    }

    @Override
    protected TrueCellValidator getThis() {
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

  private class FalseCellValidator extends CellValidatorAdapter<FalseCellValidator> {

    private List<String> hitValidators;

    private String group;

    FalseCellValidator(List<String> hitValidators) {
      this.hitValidators = hitValidators;
    }

    @Override
    public FalseCellValidator matchField(String matchField) {
      this.group = matchField;
      return super.matchField(matchField);
    }

    @Override
    protected FalseCellValidator getThis() {
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

  static class TestCellValidator extends CellValidatorAdapter<TestCellValidator> {

    private Counter counter;

    TestCellValidator() {
    }

    TestCellValidator(Counter counter) {
      this.counter = counter;
    }

    @Override
    protected TestCellValidator getThis() {
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

  static class TestRowValidator extends RowValidatorAdapter {

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

  static class Counter {
    private int count = 0;

    void hit() {
      count++;
    }

    int hitTime() {
      return count;
    }
  }

  static Sheet getSheet() {
    Sheet baseSheet = TestFactory.createSheet();
    Sheet sheet = new SheetBean();
    sheet.addRow(baseSheet.getRow(1));
    sheet.addRow(baseSheet.getRow(2));
    return sheet;
  }
}