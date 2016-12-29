package excel.engine.importer.template;

import excel.engine.ExcelConstants;
import excel.engine.exporter.extractor.BooleanZhExtractor;
import excel.engine.exporter.composer.DefaultExcelComposerEngine;
import excel.engine.exporter.composer.ExcelComposerEngine;
import excel.engine.model.excel.Row;
import excel.engine.model.excel.Sheet;
import excel.engine.importer.processor.ObjectFactory;
import excel.engine.importer.processor.ObjectProcessorEngine;
import excel.engine.importer.processor.ObjectProcessorListener;
import excel.engine.importer.setter.BooleanValueSetter;
import excel.engine.importer.setter.LocalDateValueSetter;
import excel.engine.importer.validator.ExcelValidatorEngine;
import excel.engine.importer.validator.cell.BooleanValidator;
import excel.engine.importer.validator.cell.IntValidator;
import excel.engine.importer.validator.cell.LocalDateValidator;
import excel.engine.importer.validator.sheet.FieldScopeValidator;
import excel.engine.importer.validator.sheet.RequireFieldValidator;
import org.apache.poi.util.TempFile;
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 5/3/16.
 */
public class DefaultExcelTemplateEngineTest {

  @Test
  public void testProcess() throws Exception {
    URL resource = this.getClass().getResource("test" + ExcelConstants.SUFFIX_XLSX);
    File excel = new File(resource.getFile());

    DefaultExcelTemplateEngine defaultExcelTemplateFactory = new DefaultExcelTemplateEngine(excel);

    ExcelValidatorEngine excelValidatorEngine = defaultExcelTemplateFactory.getExcelValidatorEngine();

    excelValidatorEngine.addSheetValidator(new FieldScopeValidator(new String[]{"student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool"}));
    excelValidatorEngine.addSheetValidator(new RequireFieldValidator(new String[]{"student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool"}));

    excelValidatorEngine.addCellValidator(
        new LocalDateValidator("student.enrollDate", "yyyy-MM-dd"),
        new BooleanValidator("student.inSchool"),
        new IntValidator("student.age")
    );

    assertTrue(excelValidatorEngine.valid());

    ObjectProcessorEngine objectProcessorEngine = defaultExcelTemplateFactory.getObjectProcessorEngine();

    objectProcessorEngine.addFieldValueSetter(
        new LocalDateValueSetter("student.enrollDate", "yyyy-MM-dd"),
        new BooleanValueSetter("student.inSchool")
    );

    objectProcessorEngine.addObjectFactory(new StudentObjectFactoryTest());
    objectProcessorEngine.process();
  }

  @Test
  public void testExport() throws IOException {

    StudentTest s1 = new StudentTest();
    s1.setAge(18);
    s1.setCode("111111");
    s1.setName("std1");
    s1.setEnrollDate(LocalDate.now());
    s1.setInSchool(true);

    StudentTest s2 = new StudentTest();
    s2.setAge(18);
    s2.setCode("222222");
    s2.setName("std2");
    s2.setEnrollDate(LocalDate.now());
    s2.setInSchool(true);

    List<StudentTest> list = new ArrayList<>();
    list.add(s1);
    list.add(s2);

    File file = TempFile.createTempFile("test", ExcelConstants.SUFFIX_XLSX);


    ExcelComposerEngine excelComposerEngine = new DefaultExcelComposerEngine();

    excelComposerEngine.addFieldValueExtractor(new BooleanZhExtractor("student.inSchool"));

//    excelComposerEngine.setData(list);
//    excelComposerEngine.setFields("student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool");
//    excelComposerEngine.setTitles("学号", "年龄", "姓名", "入学日期", "是否在校");

    excelComposerEngine.write(file);
  }

  public class StudentObjectProcessorListenerTest implements ObjectProcessorListener {

    @Override
    public void beforeSheet(Sheet sheet, List<Object> objects) {

    }

    @Override
    public void beforeRow(Row row, Object object) {

    }

    @Override
    public void afterRow(Row row, Object object) {

    }

    @Override
    public void afterSheet(Sheet sheet, List<Object> objects) {

      assertEquals(objects.size(), 2);

      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd");

      StudentTest model1 = (StudentTest) objects.get(0);
      StudentTest model2 = (StudentTest) objects.get(1);
      assertEquals(model1.getCode(), "111111");
      assertEquals(model2.getCode(), "2222");

      assertEquals(model1.getName(), "std1");
      assertEquals(model2.getName(), "std2");

      assertEquals(model1.getAge(), new Integer(18));
      assertEquals(model2.getAge(), new Integer(18));

      assertTrue(model1.isInSchool());
      assertTrue(model2.isInSchool());

      assertEquals(model1.getEnrollDate(), dateTimeFormatter.parseLocalDate("2015-09-02"));
      assertEquals(model2.getEnrollDate(), dateTimeFormatter.parseLocalDate("2015-09-02"));

    }

    @Override
    public int getSheetIndex() {
      return 1;
    }
  }

  public class StudentObjectFactoryTest implements ObjectFactory {

    @Override
    public Object create(Row row) {
      return new StudentTest();
    }

    @Override
    public int getSheetIndex() {
      return 1;
    }
  }

  public class StudentTest {

    private String code;

    private String name;

    private Integer age;

    private LocalDate enrollDate;

    private boolean inSchool;

    public String getCode() {
      return code;
    }

    public void setCode(String code) {
      this.code = code;
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    public Integer getAge() {
      return age;
    }

    public void setAge(Integer age) {
      this.age = age;
    }

    public LocalDate getEnrollDate() {
      return enrollDate;
    }

    public void setEnrollDate(LocalDate enrollDate) {
      this.enrollDate = enrollDate;
    }

    public boolean isInSchool() {
      return inSchool;
    }

    public void setInSchool(boolean inSchool) {
      this.inSchool = inSchool;
    }
  }

}