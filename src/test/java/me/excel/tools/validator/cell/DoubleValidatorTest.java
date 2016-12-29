package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class DoubleValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    DoubleValidator doubleValidator = new DoubleValidator("person.double");

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.double");
    assertTrue(doubleValidator.valid(cell));

    CellBean cell2 = new CellBean(1, 1, "asdasd");
    cell2.setField("person.double");
    assertFalse(doubleValidator.valid(cell2));

    CellBean cell1 = new CellBean(1, 1, "1");
    cell1.setField("person.double");
    assertTrue(doubleValidator.valid(cell1));
  }

}