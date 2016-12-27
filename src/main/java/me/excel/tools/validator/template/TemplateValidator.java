package me.excel.tools.validator.template;

import me.excel.tools.model.message.TemplateValidateMessage;

/**
 * template validator
 * <p>
 * Created by hanwen on 2016/12/26.
 */
public interface TemplateValidator {

  /**
   * @return validate error message
   */
  TemplateValidateMessage getErrorMessage();
}
