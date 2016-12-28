package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    NumberScaleRangeValidator numberValidator = new NumberScaleRangeValidator("person.numberRange", 1, 2);

    CellBean cell = new CellBean(1, 1, "person.numberRange", "");
    assertTrue(numberValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "person.numberRange", "dasdasd");
    assertFalse(numberValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "person.numberRange", "1");
    assertFalse(numberValidator.validate(cell2));

    CellBean cell3 = new CellBean(1, 1, "person.numberRange", "1.22");
    assertTrue(numberValidator.validate(cell3));
  }

}