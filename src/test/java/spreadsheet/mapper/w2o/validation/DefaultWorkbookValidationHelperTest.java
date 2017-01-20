package spreadsheet.mapper.w2o.validation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.model.core.WorkbookBean;
import spreadsheet.mapper.model.meta.WorkbookMeta;
import spreadsheet.mapper.model.meta.WorkbookMetaBean;
import spreadsheet.mapper.w2o.validation.validator.workbook.SheetSizeValidator;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static spreadsheet.mapper.w2o.validation.DefaultSheetValidationHelperTest.getSheet;

/**
 * Created by hanwen on 2017/1/19.
 */
@Test(dependsOnGroups = "sheetValidationHelperTest")
public class DefaultWorkbookValidationHelperTest {

  private static Logger LOGGER = LoggerFactory.getLogger(DefaultWorkbookValidationHelperTest.class);

  @BeforeClass
  public void before() {
    LOGGER.debug("-------------------starting test workbook validation helper-------------------");
  }

  @Test
  public void testValid() throws Exception {

    DefaultSheetValidationHelperTest.Counter counter = new DefaultSheetValidationHelperTest.Counter();

    WorkbookValidationHelper workbookValidationHelper = new DefaultWorkbookValidationHelper();

    SheetValidationHelper sheetValidationHelper = new DefaultSheetValidationHelper();
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("int1"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("int2"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("long1"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("long2"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("float1"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("float2"));
    sheetValidationHelper.addCellValidator(new DefaultSheetValidationHelperTest.TestCellValidator(counter).matchField("string"));

    sheetValidationHelper.addRowValidator(new DefaultSheetValidationHelperTest.TestRowValidator(counter).group("int1"));
    sheetValidationHelper.addRowValidator(new DefaultSheetValidationHelperTest.TestRowValidator(counter).group("int2"));


    Workbook workbook = new WorkbookBean();
    workbook.addSheet(getSheet());
    workbook.addSheet(getSheet());

    WorkbookMeta workbookMeta = new WorkbookMetaBean();
    workbookMeta.addSheetMeta(TestFactory.createSheetMeta(true));
    workbookMeta.addSheetMeta(TestFactory.createSheetMeta(true));

    boolean result = workbookValidationHelper
        .addWorkbookValidator(new SheetSizeValidator().size(2))
        .addSheetValidationHelper(sheetValidationHelper)
        .addSheetValidationHelper(sheetValidationHelper)
        .valid(workbook, workbookMeta);

    assertTrue(result);
    assertEquals(counter.hitTime(), (7 + 2) * 2);
  }

}