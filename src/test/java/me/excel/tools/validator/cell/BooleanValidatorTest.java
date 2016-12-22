package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class BooleanValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    DoubleValidator booleanValidator = new DoubleValidator("person.bool");
    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.bool", "");
    assertTrue(booleanValidator.validate(cell));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.bool", "asdasd");
    assertFalse(booleanValidator.validate(cell2));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.bool", "1");
    assertTrue(booleanValidator.validate(cell1));

  }

}