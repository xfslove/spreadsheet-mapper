package me.excel.tools.factory;

import me.excel.tools.generator.UserFileGenerator;
import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.UserFileValidator;

import java.io.IOException;
import java.util.Set;

/**
 * template factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTemplate {

  /**
   * 得到file中field字段的所有值
   *
   * @param field
   * @return
   * @throws IOException
   */
  Set<String> getCellValuesOfField(String field);

  /**
   * 获得模板对应的file factory
   *
   * @return
   */
  UserFileGenerator getUserFileGenerator();

  UserFileValidator getUserFileValidator();

  /**
   * 获得模板对应的file importer
   *
   * @return
   */
  UserFileImporter getUserFileImporter();
}
