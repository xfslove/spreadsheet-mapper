package spreadsheet.mapper.w2o.validation.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/4.
 */
public class UniqueValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {


    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();

    CellValidator[] validator = new UniqueValidator().matchField("test.string").end();

    assertEquals(validator.length, 1);

    assertTrue(validator[0].valid(cellMap1.get("test.string"), fieldMetaMap.get("test.string")));
    assertFalse(validator[0].valid(cellMap2.get("test.string"), fieldMetaMap.get("test.string")));

    assertTrue(validator[0].valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator[0].valid(cellMap2.get("test.int1"), fieldMetaMap.get("test.int1")));

    assertTrue(validator[0].valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator[0].valid(cellMap2.get("test.int2"), fieldMetaMap.get("test.int2")));

    assertTrue(validator[0].valid(cellMap1.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal")));
    assertTrue(validator[0].valid(cellMap2.get("test.bigDecimal"), fieldMetaMap.get("test.bigDecimal")));

    assertTrue(validator[0].valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));
    assertTrue(validator[0].valid(cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}