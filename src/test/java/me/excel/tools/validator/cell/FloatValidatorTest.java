package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
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

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.float", "");
    assertTrue(floatValidator.validate(cell));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.float", "asdasd");
    assertFalse(floatValidator.validate(cell2));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.float", "1");
    assertTrue(floatValidator.validate(cell1));
  }

}