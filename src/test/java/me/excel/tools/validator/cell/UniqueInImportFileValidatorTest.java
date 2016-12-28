package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class UniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    UniqueInImportFileValidator uniqueInImportFileValidator = new UniqueInImportFileValidator("person.unique");

    CellBean e1 = new CellBean(1, 1, "person.unique", "1");
    CellBean e2 = new CellBean(2, 1, "person.unique", "2");
    CellBean e3 = new CellBean(3, 1, "person.unique", "3");
    CellBean e4 = new CellBean(4, 1, "person.unique", "4");
    CellBean e5 = new CellBean(5, 1, "person.unique", "5");
    CellBean e6 = new CellBean(6, 1, "person.unique", "1");

    assertTrue(uniqueInImportFileValidator.validate(e1));
    assertTrue(uniqueInImportFileValidator.validate(e2));
    assertTrue(uniqueInImportFileValidator.validate(e3));
    assertTrue(uniqueInImportFileValidator.validate(e4));
    assertTrue(uniqueInImportFileValidator.validate(e5));
    assertFalse(uniqueInImportFileValidator.validate(e6));

  }

}