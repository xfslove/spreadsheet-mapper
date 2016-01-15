package me.excel.tools.factory;

import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.FieldValidator;
import me.excel.tools.validator.UserFileValidator;

import java.util.List;

/**
 * template factory
 *
 * Created by hanwen on 15-12-16.
 */
public interface FileTemplate {

  /**
   * 设置支持的field范围
   *
   * @param fields
   */
  void setFieldScope(String... fields);

  /**
   * 设置必须的field范围
   *
   * @param fields
   */
  void setRequiredFields(String... fields);

  /**
   * 设置最少需要的field数量
   *
   * @param count
   */
  void setMinFieldCount(int count);

  /**
   * 增加验证器
   *
   * @param validators
   */
  void addValidator(FieldValidator... validators);

  List<String> getFieldScope();

  List<String> getRequiredFields();

  List<FieldValidator> getValidators();

  /**
   * 获得模板对应的file factory
   *
   * @return
   */
  UserFileFactory getUserFileFactory();

  UserFileValidator getUserFileValidator();

  /**
   * 获得模板对应的file importer
   *
   * @return
   */
  UserFileImporter getUserFileImporter();
}
