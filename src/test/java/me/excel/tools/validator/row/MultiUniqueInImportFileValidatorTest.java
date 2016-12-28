package me.excel.tools.validator.row;

import me.excel.tools.model.excel.CellBean;
import me.excel.tools.model.excel.RowBean;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashSet;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class MultiUniqueInImportFileValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    MultiUniqueInImportFileValidator multiUniqueInImportFileValidator = new MultiUniqueInImportFileValidator(new HashSet<>(Arrays.asList("person.name", "person.age")));

    RowBean r1 = new RowBean(1);
    RowBean r2 = new RowBean(2);
    RowBean r3 = new RowBean(3);

    CellBean e1 = new CellBean(1, 1, "1");
    CellBean e2 = new CellBean(1, 2, "1");
    e1.setField("person.name");
    e2.setField("person.age");
    r1.addCell(e1);
    r1.addCell(e2);

    CellBean e3 = new CellBean(2, 1, "1");
    CellBean e4 = new CellBean(2, 2, "2");
    e3.setField("person.name");
    e4.setField("person.age");
    r2.addCell(e3);
    r2.addCell(e4);

    CellBean e5 = new CellBean(3, 1, "1");
    CellBean e6 = new CellBean(3, 2, "2");
    e5.setField("person.name");
    e6.setField("person.age");
    r3.addCell(e5);
    r3.addCell(e6);

    assertTrue(multiUniqueInImportFileValidator.validate(r1));
    assertTrue(multiUniqueInImportFileValidator.validate(r2));
    assertFalse(multiUniqueInImportFileValidator.validate(r3));
  }

}