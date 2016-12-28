package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class NumberScaleRangeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    NumberScaleRangeValidator numberValidator = new NumberScaleRangeValidator("person.numberRange", 1, 2);

    CellBean cell = new CellBean(1, 1, "");
    cell.setField("person.numberRange");
    assertTrue(numberValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "dasdasd");
    cell1.setField("person.numberRange");
    assertFalse(numberValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "1");
    cell2.setField("person.numberRange");
    assertFalse(numberValidator.validate(cell2));

    CellBean cell3 = new CellBean(1, 1, "1.22");
    cell3.setField("person.numberRange");
    assertTrue(numberValidator.validate(cell3));
  }

}