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
public class LocalDateValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    CellValidator validator = new LocalDateValidator().matchField("test.localDate").pattern("yyyy-MM-dd");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator.valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));

    Map<String, Cell> cellMap2 = TestFactory.createErrorCellMap();
    assertFalse(validator.valid(cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}