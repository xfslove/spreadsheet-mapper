package spread.sheet.w2o.validator.sheet;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class FieldScopeValidatorTest {

  @Test
  public void testValid() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    SheetMeta sheetMeta = TestFactory.createSheetMeta();

    Sheet sheet = TestFactory.createSheet();

    String[] fields1 = fieldMetaMap.keySet().toArray(new String[0]);

    FieldScopeValidator validator1 = new FieldScopeValidator("", fields1);
    assertTrue(validator1.valid(sheet, sheetMeta));

    String[] fields2 = new String[]{"1", "2", "3"};
    FieldScopeValidator validator2 = new FieldScopeValidator("", fields2);
    assertFalse(validator2.valid(sheet, sheetMeta));


    String[] fields3 = new String[fields1.length + 1];
    System.arraycopy(fields1, 0, fields3, 0, fields1.length);
    fields3[fields3.length - 1] = "1";

    FieldScopeValidator validator3 = new FieldScopeValidator("", fields3);
    assertTrue(validator3.valid(sheet, sheetMeta));
  }

}