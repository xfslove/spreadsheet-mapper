package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
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

    CellBean cell = new CellBean(1, 1, "person.float", "");
    assertTrue(floatValidator.validate(cell));

    CellBean cell2 = new CellBean(1, 1, "person.float", "asdasd");
    assertFalse(floatValidator.validate(cell2));

    CellBean cell1 = new CellBean(1, 1, "person.float", "1");
    assertTrue(floatValidator.validate(cell1));
  }

}