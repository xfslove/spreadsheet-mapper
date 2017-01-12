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
public class NumberValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator[] validator = new NumberValidator()
        .matchFields("test.int1", "test.int2", "test.long1", "test.long2", "test.float1", "test.float2", "test.double1", "test.double2").end();

    assertEquals(validator.length, 8);

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator[0].valid(cellMap1.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator[1].valid(cellMap1.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertTrue(validator[2].valid(cellMap1.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator[3].valid(cellMap1.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertTrue(validator[4].valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator[5].valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertTrue(validator[6].valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator[7].valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator[0].valid(cellMap2.get("test.int1"), fieldMetaMap.get("test.int1")));
    assertTrue(validator[1].valid(cellMap2.get("test.int2"), fieldMetaMap.get("test.int2")));
    assertFalse(validator[2].valid(cellMap2.get("test.long1"), fieldMetaMap.get("test.long1")));
    assertTrue(validator[3].valid(cellMap2.get("test.long2"), fieldMetaMap.get("test.long2")));
    assertFalse(validator[4].valid(cellMap2.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator[5].valid(cellMap2.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator[6].valid(cellMap2.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator[7].valid(cellMap2.get("test.double2"), fieldMetaMap.get("test.double2")));

  }

}