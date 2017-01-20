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
public class RequireValidatorTest {

  @Test
  public void testValid() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();

    CellValidator validator0 = new RequireValidator().matchField("int1");
    CellValidator validator1 = new RequireValidator().matchField("int2");
    CellValidator validator2 = new RequireValidator().matchField("long1");
    CellValidator validator3 = new RequireValidator().matchField("long2");
    CellValidator validator4 = new RequireValidator().matchField("float1");
    CellValidator validator5 = new RequireValidator().matchField("float2");
    CellValidator validator6 = new RequireValidator().matchField("double1");
    CellValidator validator7 = new RequireValidator().matchField("double2");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator0.valid(cellMap1.get("int1"), fieldMetaMap.get("int1")));
    assertTrue(validator1.valid(cellMap1.get("int2"), fieldMetaMap.get("int2")));
    assertTrue(validator2.valid(cellMap1.get("long1"), fieldMetaMap.get("long1")));
    assertTrue(validator3.valid(cellMap1.get("long2"), fieldMetaMap.get("long2")));
    assertTrue(validator4.valid(cellMap1.get("float1"), fieldMetaMap.get("float1")));
    assertTrue(validator5.valid(cellMap1.get("float2"), fieldMetaMap.get("float2")));
    assertTrue(validator6.valid(cellMap1.get("double1"), fieldMetaMap.get("double1")));
    assertTrue(validator7.valid(cellMap1.get("double2"), fieldMetaMap.get("double2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertTrue(validator0.valid(cellMap2.get("int1"), fieldMetaMap.get("int1")));
    assertFalse(validator1.valid(cellMap2.get("int2"), fieldMetaMap.get("int2")));
    assertTrue(validator2.valid(cellMap2.get("long1"), fieldMetaMap.get("long1")));
    assertFalse(validator3.valid(cellMap2.get("long2"), fieldMetaMap.get("long2")));
    assertTrue(validator4.valid(cellMap2.get("float1"), fieldMetaMap.get("float1")));
    assertFalse(validator5.valid(cellMap2.get("float2"), fieldMetaMap.get("float2")));
    assertTrue(validator6.valid(cellMap2.get("double1"), fieldMetaMap.get("double1")));
    assertFalse(validator7.valid(cellMap2.get("double2"), fieldMetaMap.get("double2")));
  }

}