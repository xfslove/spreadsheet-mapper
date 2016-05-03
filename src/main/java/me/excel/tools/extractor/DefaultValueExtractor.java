package me.excel.tools.extractor;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;


/**
 * 默认的object属性extractor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultValueExtractor.class);

  public String getStringValue(Object data, String field) {

    try {
      return BeanUtils.getProperty(data, getFieldWithoutPrefix(field));
    } catch (NestedNullException e) {
      LOGGER.trace(e.getMessage());
      return "";
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new IllegalArgumentException(e);
    }
  }

}
