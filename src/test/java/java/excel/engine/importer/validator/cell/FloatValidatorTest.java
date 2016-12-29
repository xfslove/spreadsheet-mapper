package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class FloatValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    FloatValidator floatValidator = new FloatValidator("person.float");

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.float");
    assertTrue(floatValidator.valid(cell));

    CellBean cell2 = new CellBean(1, 1, "asdasd");
    cell2.setField("person.float");
    assertFalse(floatValidator.valid(cell2));

    CellBean cell1 = new CellBean(1, 1, "1");
    cell1.setField("person.float");
    assertTrue(floatValidator.valid(cell1));
  }

}