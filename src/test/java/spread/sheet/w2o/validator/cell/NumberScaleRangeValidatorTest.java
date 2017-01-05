package spread.sheet.w2o.validator.cell;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/4.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    NumberScaleRangeValidator validator5 = new NumberScaleRangeValidator(1, 10, "test.float1", "");
    NumberScaleRangeValidator validator6 = new NumberScaleRangeValidator(1, 10, "test.float2", "");
    NumberScaleRangeValidator validator7 = new NumberScaleRangeValidator(1, 10, "test.double1", "");
    NumberScaleRangeValidator validator8 = new NumberScaleRangeValidator(1, 10, "test.double2", "");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator5.valid(cellMap1.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator6.valid(cellMap1.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator7.valid(cellMap1.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertFalse(validator8.valid(cellMap1.get("test.double2"), fieldMetaMap.get("test.double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator5.valid(cellMap2.get("test.float1"), fieldMetaMap.get("test.float1")));
    assertTrue(validator6.valid(cellMap2.get("test.float2"), fieldMetaMap.get("test.float2")));
    assertFalse(validator7.valid(cellMap2.get("test.double1"), fieldMetaMap.get("test.double1")));
    assertTrue(validator8.valid(cellMap2.get("test.double2"), fieldMetaMap.get("test.double2")));
  }

}