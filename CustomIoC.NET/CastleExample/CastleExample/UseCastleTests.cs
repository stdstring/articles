using System;
using Castle.MicroKernel.Handlers;
using Castle.MicroKernel.Registration;
using Castle.Windsor;
using NUnit.Framework;

namespace CastleExample
{
    public interface ISomeInnerService
    {
        Int32 Do();
    }

    public interface ISomeOtherInnerService
    {
        String Do();
    }

    public interface ISomeService
    {
        void Do();
    }

    public class SomeInnerServiceImpl : ISomeInnerService
    {
        public Int32 Do()
        {
            return 666;
        }
    }

    public class SomeOtherInnerServiceImpl : ISomeOtherInnerService
    {
        public String Do()
        {
            return "IDDQD";
        }
    }

    public class SomeServiceImpl : ISomeService
    {
        public SomeServiceImpl(ISomeInnerService inner)
        {
            _inner = inner;
        }

        public void Do()
        {
            _inner.Do();
        }

        private readonly ISomeInnerService _inner;
    }

    public class SomeServiceOtherImpl : ISomeService
    {
        public SomeServiceOtherImpl(ISomeOtherInnerService inner)
        {
            _inner = inner;
        }

        public void Do()
        {
            _inner.Do();
        }

        private readonly ISomeOtherInnerService _inner;
    }

    [TestFixture]
    public class UseCastleTests
    {
        [Test]
        public void UseSomeServiceImpl()
        {
            IWindsorContainer container = new WindsorContainer();
            container.Register(Component.For<ISomeService>().ImplementedBy<SomeServiceImpl>());
            container.Register(Component.For<ISomeInnerService>().ImplementedBy<SomeInnerServiceImpl>());
            ISomeService service = container.Resolve<ISomeService>();
            Assert.IsNotNull(service);
        }

        [Test]
        public void UseSomeServiceOtherImpl()
        {
            IWindsorContainer container = new WindsorContainer();
            container.Register(Component.For<ISomeService>().ImplementedBy<SomeServiceOtherImpl>());
            container.Register(Component.For<ISomeInnerService>().ImplementedBy<SomeInnerServiceImpl>());
            Assert.Throws<HandlerException>(() => container.Resolve<ISomeService>());
        }
    }
}
