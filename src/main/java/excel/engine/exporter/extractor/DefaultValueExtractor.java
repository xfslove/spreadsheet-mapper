package excel.engine.exporter.extractor;

import excel.engine.exception.ExcelProcessException;
import excel.engine.util.FieldUtils;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * default value extractor, using {@link BeanUtils#getProperty(Object, String)}
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultValueExtractor.class);

  private DefaultValueExtractor() {
  }

  public static String getStringValue(Object data, String field) {
    try {
      return BeanUtils.getProperty(data, FieldUtils.detectRealField(field));
    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }

}
