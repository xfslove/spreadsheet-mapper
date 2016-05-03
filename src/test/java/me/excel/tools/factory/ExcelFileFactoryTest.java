package me.excel.tools.factory;

import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Created by hanwen on 5/3/16.
 */
public class ExcelFileFactoryTest {

  @Test
  public void testFileFactory() throws IOException {

    StudentTest s1 = new StudentTest();
    s1.setAge(18);
    s1.setCode("111111");
    s1.setEnrollDate(new Date());
    s1.setName("std1");

    StudentTest s2 = new StudentTest();
    s1.setAge(18);
    s1.setCode("222222");
    s1.setEnrollDate(new Date());
    s1.setName("std2");

    List<StudentTest> studentTestList = new ArrayList<>();
    studentTestList.add(s1);
    studentTestList.add(s2);

    ExcelTemplate fileTemplate = new ExcelTemplate();
    ExcelFileFactory excelFileFactory = new ExcelFileFactory(fileTemplate);

    excelFileFactory.setData(studentTestList);
    excelFileFactory.setTitles("学号", "姓名", "年龄", "入学日期");
    excelFileFactory.setFields("student.code", "student.name", "student.age", "student.enrollDate");

    File file = new File("/Users/hanwen/Downloads/test.xlsx");

    excelFileFactory.generate(file);
  }

  public class StudentTest {

    private String code;

    private String name;

    private Integer age;

    private Date enrollDate;

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

    public Date getEnrollDate() {
      return enrollDate;
    }

    public void setEnrollDate(Date enrollDate) {
      this.enrollDate = enrollDate;
    }
  }

}