package spread.sheet.w2o.validator.row;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class MultiUniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Sheet sheet = TestFactory.createSheet();

    SheetMeta sheetMeta = TestFactory.createSheetMeta();

    MultiUniqueInImportFileValidator validator1 = new MultiUniqueInImportFileValidator("", new String[]{"test.int1", "test.int2"});
    assertTrue(validator1.valid(sheet.getRow(2), sheetMeta));
    assertTrue(validator1.valid(sheet.getRow(3), sheetMeta));

    MultiUniqueInImportFileValidator validator2 = new MultiUniqueInImportFileValidator("", new String[]{"test.string"});
    assertTrue(validator2.valid(sheet.getRow(2), sheetMeta));
    assertFalse(validator2.valid(sheet.getRow(3), sheetMeta));

  }

}