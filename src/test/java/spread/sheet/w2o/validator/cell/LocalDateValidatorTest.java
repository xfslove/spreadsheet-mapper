package spread.sheet.w2o.validator.cell;

import org.testng.annotations.Test;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

import java.util.Map;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/4.
 */
public class LocalDateValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    Map<String, FieldMeta> fieldMetaMap = TestFactory.createFieldMetaMap();
    LocalDateValidator validator = new LocalDateValidator("yyyy-MM-dd", "test.localDate", "");

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();
    assertTrue(validator.valid(cellMap1.get("test.localDate"), fieldMetaMap.get("test.localDate")));

    Map<String, Cell> cellMap2 = TestFactory.createCellMap2();
    assertFalse(validator.valid(cellMap2.get("test.localDate"), fieldMetaMap.get("test.localDate")));
  }

}