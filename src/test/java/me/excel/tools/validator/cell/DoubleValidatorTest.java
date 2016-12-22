package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class DoubleValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    DoubleValidator doubleValidator = new DoubleValidator("person.double");

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.double", "");
    assertTrue(doubleValidator.validate(cell));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.double", "asdasd");
    assertFalse(doubleValidator.validate(cell2));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.double", "1");
    assertTrue(doubleValidator.validate(cell1));
  }

}