package me.excel.tools.factory;

import me.excel.tools.generator.UserFileGenerator;
import me.excel.tools.extractor.BooleanZhExtractor;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.prompter.BooleanPrompter;
import me.excel.tools.prompter.PromptBuilder;
import me.excel.tools.prompter.RequiredPrompter;
import me.excel.tools.setter.BooleanValueSetter;
import me.excel.tools.setter.LocalDateValueSetter;
import me.excel.tools.validator.UserFileValidator;
import me.excel.tools.validator.cell.BooleanValidator;
import me.excel.tools.validator.cell.IntValidator;
import me.excel.tools.validator.cell.LocalDateValidator;
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
public class ExcelTemplateTest {

  @Test
  public void testProcess() throws Exception {
    URL resource = this.getClass().getResource("test.xlsx");
    File excel = new File(resource.getFile());

    ExcelTemplate excelTemplate = new ExcelTemplate(excel);

    UserFileValidator userFileValidator = excelTemplate.getUserFileValidator();

    userFileValidator.setFieldScope("student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool");
    userFileValidator.setRequiredFields("student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool");

    userFileValidator.addCellValidator(
        new LocalDateValidator("student.enrollDate", "yyyy-MM-dd"),
        new BooleanValidator("student.inSchool"),
        new IntValidator("student.age")
    );

    assertTrue(userFileValidator.validate());

    UserFileImporter userFileImporter = excelTemplate.getUserFileImporter();

    userFileImporter.addCellValueSetter(
        new LocalDateValueSetter("student.enrollDate", "yyyy-MM-dd"),
        new BooleanValueSetter("student.inSchool")
    );

    userFileImporter.setModelFactory(new StudentModelFactoryTest());
    userFileImporter.process(new StudentDataProcessorTest());
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

    File file = TempFile.createTempFile("test", ".xlsx");

    ExcelTemplate excelTemplate = new ExcelTemplate(file);

    UserFileGenerator userFileGenerator = excelTemplate.getUserFileGenerator();

    userFileGenerator.addCellPrompters(
        new PromptBuilder()
            .prompt("student.age", "整数")
            .prompt("student.enrollDate", "yyyy-MM-dd")
            .add(new RequiredPrompter("student.code"))
            .add(new RequiredPrompter("student.age"))
            .add(new RequiredPrompter("student.name"))
            .add(new BooleanPrompter("student.inSchool"))
            .build()
    );

    userFileGenerator.addValueExtractors(new BooleanZhExtractor("student.inSchool"));

    userFileGenerator.setData(list);
    userFileGenerator.setFields("student.code", "student.age", "student.name", "student.enrollDate", "student.inSchool");
    userFileGenerator.setTitles("学号", "年龄", "姓名", "入学日期", "是否在校");

    userFileGenerator.generate();
  }

  public class StudentDataProcessorTest implements DataProcessor {

    @Override
    public void preProcessing(Object model) {

    }

    @Override
    public void postProcessing(Object model) {

    }

    @Override
    public void handle(List models) {

      assertEquals(models.size(), 2);

      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd");

      StudentTest model1 = (StudentTest) models.get(0);
      StudentTest model2 = (StudentTest) models.get(1);
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
  }

  public class StudentModelFactoryTest implements ModelFactory {

    @Override
    public Object create(ExcelRow row) {
      return new StudentTest();
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