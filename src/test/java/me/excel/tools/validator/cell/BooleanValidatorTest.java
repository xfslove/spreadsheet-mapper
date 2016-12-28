package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
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
    CellBean cell = new CellBean(1, 1, "person.bool", "");
    assertTrue(booleanValidator.validate(cell));

    CellBean cell2 = new CellBean(1, 1, "person.bool", "asdasd");
    assertFalse(booleanValidator.validate(cell2));

    CellBean cell1 = new CellBean(1, 1, "person.bool", "1");
    assertTrue(booleanValidator.validate(cell1));

  }

}