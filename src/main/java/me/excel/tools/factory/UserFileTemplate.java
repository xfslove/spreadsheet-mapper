package me.excel.tools.factory;

import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.UserFileValidator;

import java.util.Set;

/**
 * import template factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTemplate {

  /**
   * 得到file中field字段的所有值
   *
   * @param field
   * @return
   */
  Set<String> getCellValuesOfField(String field);

  UserFileValidator getUserFileValidator();

  UserFileImporter getUserFileImporter();
}
