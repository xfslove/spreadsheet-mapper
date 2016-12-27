package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    NumberScaleRangeValidator numberValidator = new NumberScaleRangeValidator("person.numberRange", 1, 2);

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.numberRange", "");
    assertTrue(numberValidator.validate(cell));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.numberRange", "dasdasd");
    assertFalse(numberValidator.validate(cell1));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.numberRange", "1");
    assertFalse(numberValidator.validate(cell2));

    ExcelCellBean cell3 = new ExcelCellBean(1, 1, "person.numberRange", "1.22");
    assertTrue(numberValidator.validate(cell3));
  }

}