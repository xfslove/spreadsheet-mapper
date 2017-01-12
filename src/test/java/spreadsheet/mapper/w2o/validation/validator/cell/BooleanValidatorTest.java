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
public class BooleanValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    CellValidator[] validators = new BooleanValidator().matchFields("test.boolean1", "test.boolean2").supportedTrue("pass").supportedFalse("failure").end();

    assertEquals(validators.length, 2);

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validators[0].valid(cellMap1.get("test.boolean1"), fieldMetaMap.get("test.boolean1")));
    assertTrue(validators[1].valid(cellMap1.get("test.boolean2"), fieldMetaMap.get("test.boolean2")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validators[0].valid(cellMap2.get("test.boolean1"), fieldMetaMap.get("test.boolean1")));
    assertTrue(validators[1].valid(cellMap2.get("test.boolean2"), fieldMetaMap.get("test.boolean2")));
  }

}