package spreadsheet.mapper.w2o.validation.validator.cell;

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
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator validator0 = new NumberScaleRangeValidator().matchField("float1").gte(1).lte(10);
    CellValidator validator1 = new NumberScaleRangeValidator().matchField("float2").gte(1).lte(10);
    CellValidator validator2 = new NumberScaleRangeValidator().matchField("double1").gte(1).lte(10);
    CellValidator validator3 = new NumberScaleRangeValidator().matchField("double2").gte(1).lte(10);

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator0.valid(cellMap1.get("float1"), fieldMetaMap.get("float1")));
    assertTrue(validator1.valid(cellMap1.get("float2"), fieldMetaMap.get("float2")));
    assertFalse(validator2.valid(cellMap1.get("double1"), fieldMetaMap.get("double1")));
    assertFalse(validator3.valid(cellMap1.get("double2"), fieldMetaMap.get("double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator0.valid(cellMap2.get("float1"), fieldMetaMap.get("float1")));
    assertTrue(validator1.valid(cellMap2.get("float2"), fieldMetaMap.get("float2")));
    assertFalse(validator2.valid(cellMap2.get("double1"), fieldMetaMap.get("double1")));
    assertTrue(validator3.valid(cellMap2.get("double2"), fieldMetaMap.get("double2")));
  }

}