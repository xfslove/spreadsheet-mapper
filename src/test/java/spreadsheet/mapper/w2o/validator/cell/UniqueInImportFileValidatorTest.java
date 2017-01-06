package spreadsheet.mapper.w2o.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class UniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {


    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    UniqueInImportFileValidator validator = new UniqueInImportFileValidator("test.string", "");

    assertTrue(validator.valid(cellMap1.get("test.string"), fieldMetaMap.get("test.string")));
    assertFalse(validator.valid(cellMap2.get("test.string"), fieldMetaMap.get("test.string")));

    assertTrue(validator.valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator.valid(cellMap2.get("test.int1"), fieldMetaMap.get("test.int1")));

    assertTrue(validator.valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator.valid(cellMap2.get("test.int2"), fieldMetaMap.get("test.int2")));

    assertTrue(validator.valid(cellMap1.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal")));
    assertTrue(validator.valid(cellMap2.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal")));

    assertTrue(validator.valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));
    assertTrue(validator.valid(cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}