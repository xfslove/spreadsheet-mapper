package spreadsheet.mapper.w2o.validation.validator.cell;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator[] validator = new NumberScaleRangeValidator().matchFields("test.float1", "test.float2", "test.double1", "test.double2").gte(1).lte(10).end();

    assertEquals(validator.length, 4);

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator[0].valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator[1].valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator[2].valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator[3].valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator[0].valid(cellMap2.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator[1].valid(cellMap2.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator[2].valid(cellMap2.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator[3].valid(cellMap2.get("test.double2"), fieldMetaMap.get("test.double2")));
  }

}