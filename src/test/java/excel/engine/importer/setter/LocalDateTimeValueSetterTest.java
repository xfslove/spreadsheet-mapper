package excel.engine.importer.setter;

import excel.engine.importer.template.TestPersonModel;
import excel.engine.model.excel.CellBean;
import org.joda.time.LocalDate;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateTimeValueSetterTest {

  @Test
  public void testSet() throws Exception {

    TestPersonModel model = new TestPersonModel();

    LocalDateValueSetter localDateValueSetter = new LocalDateValueSetter("person.birthday", "yyyy-MM-dd");
    CellBean cell = new CellBean(1, 1,  "1984-11-22");
    cell.setField("person.birthday");
    localDateValueSetter.set(model, cell);

    assertEquals(model.getBirthday(), new LocalDate(1984, 11, 22));
  }

}