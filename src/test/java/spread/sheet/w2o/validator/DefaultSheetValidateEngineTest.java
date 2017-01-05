package spread.sheet.w2o.validator;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.*;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMetaBean;
import spread.sheet.w2o.validator.cell.CellValidatorAdapter;

import java.util.Map;

import static org.testng.Assert.assertFalse;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetValidateEngineTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultSheetValidateEngineTest.class);

  @Test
  public void testCyclingDependencyException() {

    SheetValidateEngine sheetValidateEngine = new DefaultSheetValidateEngine().sheetMeta(new SheetMetaBean(2)).sheet(new SheetBean());

    /*
       no cycle
       a -> b, c
       b -> d
       c -> f
       d -> e, f
       e, f -> g
     */
    sheetValidateEngine.cellValidator(
        new TestValidator("a", new String[]{"b"}),
        new TestValidator("a", new String[]{"c"}),
        new TestValidator("a", new String[]{}),
        new TestValidator("a", new String[]{}),
        new TestValidator("b", new String[]{"d"}),
        new TestValidator("c", new String[]{"f"}),
        new TestValidator("c", new String[]{}),
        new TestValidator("c", new String[]{}),
        new TestValidator("c", new String[]{}),
        new TestValidator("d", new String[]{"e", "f"}),
        new TestValidator("e", new String[]{"g"}),
        new TestValidator("f", new String[]{}),
        new TestValidator("f", new String[]{"g"}),
        new TestValidator("f", new String[]{}),
        new TestValidator("g", new String[]{})
    );

    boolean cycling = false;
    try {
      sheetValidateEngine.valid();
    } catch (WorkbookValidateException e) {
      LOGGER.info(ExceptionUtils.getStackTrace(e));
      if (StringUtils.contains(e.getMessage(), "cycling")) {
        cycling = true;
      }
    }
    assertFalse(cycling);
  }

  private class TestValidator extends CellValidatorAdapter {

    TestValidator(String matchField, String[] dependsOn) {
      super(matchField, "", dependsOn);
    }

    @Override
    protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
      return true;
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