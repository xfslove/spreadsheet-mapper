package me.excel.tools.utils;

import me.excel.tools.FieldUtils;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;


/**
 * 默认的object属性extractor
 *
 * Created by hanwen on 15-12-16.
 */
public class ReflectionValueExtractor implements ValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(ReflectionValueExtractor.class);

  @Override
  public String getStringValue(Object data, String field) {

    String fieldWithoutPrefix;

    if (field.contains(FieldUtils.BUSINESS_KEY_PREFIX)) {
      fieldWithoutPrefix = getFieldWithoutPrefix(FieldUtils.getBusinessKeyField(field));
    } else {
      fieldWithoutPrefix = getFieldWithoutPrefix(field);
    }

    try {
      return BeanUtils.getProperty(data, fieldWithoutPrefix);
    } catch (NestedNullException e) {
      LOGGER.trace(e.getMessage());
      return "";
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new IllegalArgumentException(e);
    }
  }

}
