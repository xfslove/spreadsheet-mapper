package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class DoubleValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    DoubleValidator doubleValidator = new DoubleValidator("person.double");

    CellBean cell = new CellBean(1, 1, "person.double", "");
    assertTrue(doubleValidator.validate(cell));

    CellBean cell2 = new CellBean(1, 1, "person.double", "asdasd");
    assertFalse(doubleValidator.validate(cell2));

    CellBean cell1 = new CellBean(1, 1, "person.double", "1");
    assertTrue(doubleValidator.validate(cell1));
  }

}