package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class NumberValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    NumberValidator numberValidator = new NumberValidator("person.number");

    CellBean cell = new CellBean(1, 1, "person.number", "");
    assertTrue(numberValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "person.number", "dasdasd");
    assertFalse(numberValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "person.number", "1");
    assertTrue(numberValidator.validate(cell2));
  }

}