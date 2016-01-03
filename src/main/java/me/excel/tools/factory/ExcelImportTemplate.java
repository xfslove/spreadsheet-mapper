package me.excel.tools.factory;

import me.excel.tools.importer.ExcelFileImporter;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.AbstractFieldValidator;
import me.excel.tools.validator.ExcelFileFileValidator;
import me.excel.tools.validator.FieldValidator;
import me.excel.tools.validator.UserFileValidator;

import java.util.ArrayList;
import java.util.List;

/**
 * excel 模板工厂, 用法如下:<br/>
 * <pre>
 *  {@link ExcelImportTemplate}
 *  ImportTemplate studentImportTemplate = new ExcelImportTemplate();
 *
 *  设置支持的field范围, 范围之外的field是不允许的
 *  studentImportTemplate.setFieldScope("student.name", "student.code", "student.age", "student.mobile", "student.enrollDate");
 *
 *  设置必须包含的field
 *  studentImportTemplate.setRequiredFields("student.name", "student.code", "student.age", "student.mobile", "student.enrollDate");
 *
 *  设置最少的field的数量
 *  studentImportTemplate.setMinFieldCount(3);
 *
 *  设置validators, {@link AbstractFieldValidator}
 *  // FieldValidator
 *  //    matchField   对应的field
 *  //    prompt  校验要求，在模板上要写出来的
 *  //    errorMsg  错误信息的template，支持 {0} 替换, 替换的是field， 需继承　{@link AbstractFieldValidator#getErrorMessage()}
 *  //    validate(ExcelCell excelCell) return boolean
 *  studentImportTemplate.addValidator(new RequiredValidator("student.code"));
 *  studentImportTemplate.addValidator(new RequiredValidator("student.name"));
 *  studentImportTemplate.addValidator(new DateFormatValidator("student.enrollDate", "yyyy-MM-dd"));
 *  studentImportTemplate.addValidator(new RegexFormatValidator("student.mobile", "\\d{11}"));
 *  studentImportTemplate.addValidator(new IntFormatValidator("student.age"));
 *
 *  生成模板. 根据validator在模板上生成了校验要求
 *  {@link UserFileFactory}
 *  UserFileFactory userFileFactory = studentImportTemplate.getUserFileFactory();
 *  userFileFactory.setFields("student.name", "student.code", "student.age", "student.mobile", "student.enrollDate");
 *
 *  List<Student> students = new ArrayList<>();
 *  userFileFactory.setDatas(students);
 *  生成导入模板
 *  File userFile = new File("/home/hanwen/tmp/template.xlsx");
 *  userFileFactory.generate(userFile);
 *
 *  ///////////
 *  ///////////
 *  ///////////　上传上来的文件
 *  File excel = new File("/home/hanwen/tmp/student.xlsx");
 *  UserFileValidator userFileValidator = studentImportTemplate.getUserFileValidator();
 *  boolean valid = userFileValidator.validate(excel);
 *  if (!valid) {
 *
 *  将错误信息写入到上传上来的文件中
 *  userFileValidator.writeFailureMessageComments(excel);
 *
 *  } else {
 *
 *  UserFileImporter userFileImporter = studentImportTemplate.getUserFileImporter();
 *
 *  自定义model factory
 *  StudentModelFactory modelFactory = new StudentModelFactory(Student.class);
 *  userFileImporter.setModelFactory(modelFactory);
 *
 *  自定义特殊的field value setter, 默认提供string, int, double, boolean类型field的value setter
 *  {@link me.excel.tools.utils.AbstractFieldValueSetter}
 *  userFileImporter.addFieldValueSetter(new DateValueSetter("student.enrollDate", "yyyy-MM-dd",
 *    (s, enrollDate) -> {
 *      Student student = (Student) s;
 *      student.setEnrollDate(enrollDate);
 *    }
 *  ));
 *
 *  自定义data processor
 *  {@link me.excel.tools.processor.DataProcessor}
 *  DataProcessor modelDataProcessor = new StudentDataProcessor();
 *
 *  只支持单sheet的导入，多sheet不支持
 *  userFileImporter.process(userFile, modelDataProcessor);
 * }
 * </pre>
 *
 * Created by hanwen on 15-12-16.
 */
public class ExcelImportTemplate implements ImportTemplate {

  protected List<String> fieldScope = new ArrayList<>();

  protected List<String> requiredFields = new ArrayList<>();

  protected int minFieldCount;

  protected List<FieldValidator> validators = new ArrayList<>();

  protected UserFileFactory userFileFactory;

  protected UserFileValidator userFileValidator;

  protected UserFileImporter userFileImporter;

  public ExcelImportTemplate() {
    this.userFileFactory = new ExcelFileFactory(this);
    this.userFileValidator = new ExcelFileFileValidator(this);
    this.userFileImporter = new ExcelFileImporter();
  }

  @Override
  public void setFieldScope(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("field scope is null");
    }

    for (String field : fields) {
      this.fieldScope.add(field);
    }
  }

  @Override
  public void setRequiredFields(String... fields) {
    if (fields == null) {
      throw new IllegalArgumentException("required field is null");
    }

    for (String field : fields) {
      this.requiredFields.add(field);
    }
  }

  @Override
  public void setMinFieldCount(int count) {
    this.minFieldCount = count;
  }

  @Override
  public void addValidator(FieldValidator... validators) {
    if (validators == null) {
      return;
    }
    for (FieldValidator validator : validators) {
      this.validators.add(validator);
    }
  }

  @Override
  public List<String> getFieldScope() {
    return this.fieldScope;
  }

  @Override
  public List<String> getRequiredFields() {
    return this.requiredFields;
  }

  @Override
  public List<FieldValidator> getValidators() {
    return this.validators;
  }

  @Override
  public UserFileFactory getUserFileFactory() {
    return this.userFileFactory;
  }

  @Override
  public UserFileValidator getUserFileValidator() {
    return this.userFileValidator;
  }

  @Override
  public UserFileImporter getUserFileImporter() {
    return this.userFileImporter;
  }
}
