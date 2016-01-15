package me.excel.tools.utils;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.utils.FieldUtils.getFieldWithoutPrefix;

/**
 * 默认的object属性extractor
 *
 * Created by hanwen on 15-12-16.
 */
public class ReflectionValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(ReflectionValueExtractor.class);

  public String getStringValue(Object data, String field) {

    String fieldWithoutPrefix = getFieldWithoutPrefix(field);
    try {
      return BeanUtils.getProperty(data, fieldWithoutPrefix);
    } catch (NestedNullException e) {
      LOGGER.warn(ExceptionUtils.getStackTrace(e));
      return "";
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new RuntimeException(e);
    }
  }

}
