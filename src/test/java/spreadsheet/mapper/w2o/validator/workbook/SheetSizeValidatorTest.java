package spreadsheet.mapper.w2o.validator.workbook;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.SheetBean;
import spreadsheet.mapper.model.core.Workbook;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class SheetSizeValidatorTest {

  @Test
  public void testValid() throws Exception {

    SheetSizeValidator sheetSizeValidator = new SheetSizeValidator(2, "");

    Workbook workbook = TestFactory.createWorkbook();

    assertFalse(sheetSizeValidator.valid(workbook));

    workbook.addSheet(new SheetBean());

    assertTrue(sheetSizeValidator.valid(workbook));

  }

}