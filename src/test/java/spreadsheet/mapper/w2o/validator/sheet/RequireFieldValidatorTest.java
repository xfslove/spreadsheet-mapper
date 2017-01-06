package spreadsheet.mapper.w2o.validator.sheet;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class RequireFieldValidatorTest {

  @Test
  public void testValid() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    SheetMeta sheetMeta = TestFactory.createSheetMeta(true);

    Sheet sheet = TestFactory.createSheet();

    String[] fields1 = fieldMetaMap.keySet().toArray(new String[0]);

    RequireFieldValidator validator1 = new RequireFieldValidator("", fields1);
    assertTrue(validator1.valid(sheet, sheetMeta));

    String[] fields2 = new String[]{"1", "2", "3"};
    RequireFieldValidator validator2 = new RequireFieldValidator("", fields2);
    assertFalse(validator2.valid(sheet, sheetMeta));


    String[] fields3 = new String[fields1.length + 1];
    System.arraycopy(fields1, 0, fields3, 0, fields1.length);
    fields3[fields3.length - 1] = "1";

    RequireFieldValidator validator3 = new RequireFieldValidator("", fields3);
    assertFalse(validator3.valid(sheet, sheetMeta));

  }

}