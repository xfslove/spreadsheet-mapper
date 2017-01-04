package spread.sheet.w2o.validator.row;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.RowBean;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.model.meta.SheetMetaBean;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class MultiUniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createCellMap2();

    Row r1 = new RowBean(1);
    for (Cell cell : cellMap1.values()) {
      r1.addCell(cell);
    }
    Row r2 = new RowBean(2);
    for (Cell cell : cellMap2.values()) {
      r2.addCell(cell);
    }

    SheetMeta sheetMeta = new SheetMetaBean(1, 1);
    for (FieldMeta fieldMeta : fieldMetaMap.values()) {
      sheetMeta.addFieldMeta(fieldMeta);
    }

    MultiUniqueInImportFileValidator validator1 = new MultiUniqueInImportFileValidator("", new String[]{"test.int1", "test.int2"});
    assertTrue(validator1.valid(r1, sheetMeta));
    assertTrue(validator1.valid(r2, sheetMeta));

    MultiUniqueInImportFileValidator validator2 = new MultiUniqueInImportFileValidator("", new String[]{"test.bigDecimal"});
    assertTrue(validator2.valid(r1, sheetMeta));
    assertFalse(validator2.valid(r2, sheetMeta));

  }

}